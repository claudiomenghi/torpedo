/**
  * Copyright (C) 2017  Alessandro M. Rizzi <alessandromaria.rizzi@polimi.it>
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Affero General Public License as
  * published by the Free Software Foundation, either version 3 of the
  * License, or (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Affero General Public License for more details.
  *
  * You should have received a copy of the GNU Affero General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  *
  */
package thrive.pks

import thrive.insights._
import thrive.ltl._
import thrive.mc._
import thrive.solver.{SATISFIABLE, Solver, SolverInstance, UNSATISFIABLE}
import thrive.utilities.Writer

import scala.xml.{Node, XML}

case class PartialKripkeStructure(name : String, states : List[State], transitions : List[Transition]) {

  private val initialStates : Set[State] = states.filter(_.isInitial).toSet;

  private final val USE_SLOW_STATE_PREDICATE = true;

  private val atomicFormulae = states.flatMap(_.atomicFormulae).toSet;

  private val p = Array.range(0, states.size).map(i => states(i) -> AtomicFormula("s" + i)).toMap;

  private val transitionMap = transitions.groupBy(_.from).map(x => x._1 -> x._2.map(_.to));

  require(states.toSet == transitionMap.keySet, "There is a state without outgoing transitions!");

  private def distinct : Seq[Clause] = {
    val sp = p.values.toArray;
    val c = Array.range(0, sp.length - 1).map(i => sp(i) -> !Disjunction(Array.range(i+1, sp.length).map(sp)).simplify);
    (c :+ Disjunction(sp)).map(G).map(Clause(_, PKSConstraint));
  }

  private def transitionPredicate(from : State, to : Seq[State]) = G(p(from) -> X(Disjunction(to.map(p)).simplify));

  private def transitionPredicate : Seq[Clause] =
    transitionMap.map(t => Clause(transitionPredicate(t._1, t._2), StateTransition(t._1, t._2))).toSeq;

  private def initialPredicate : Clause =
    Clause(Disjunction(initialStates.toSeq.map(s => p(s))).simplify, InitialState(initialStates.toSeq));

  private def fastStatePredicate(f : AtomicFormula => Literal) : Seq[Clause] =
    states.map { state =>
      val (literals, dependOnMaybe) = state.approximation(atomicFormulae, f).unzip;
      val insight = StatePredicate(state, literals.map(_.original).toSet, dependOnMaybe.exists(x => x));
      Clause(G(p(state) -> Conjunction(literals).simplify), insight);
    }

  private def slowStatePredicate(f : AtomicFormula => Literal) : Seq[Clause] =
    for{
      s <- states;
      (lit, maybeDependent) <- s.approximation(atomicFormulae, f)
    }
      yield Clause(G(p(s) -> lit), StatePredicate(s, Set(lit.original), maybeDependent));

  private def statePredicate(f : AtomicFormula => Literal) : Seq[Clause] =
    if(USE_SLOW_STATE_PREDICATE)
      slowStatePredicate(f);
    else
      fastStatePredicate(f);

  private def transformProperty(property : LtlFormula) : Clause = Clause(!property.complementClosed, Property);

  private def optimistic(property : LtlFormula) : Seq[Clause] =
    distinct ++ transitionPredicate ++ statePredicate(a => a) ++ Seq(initialPredicate, transformProperty(property));

  private def pessimistic(property : LtlFormula) : Seq[Clause] =
    distinct ++ transitionPredicate ++ statePredicate(a => !a) ++ Seq(initialPredicate, transformProperty(property));

  private def writePTLTFile(properties : Seq[Clause], filename : String) : Unit =
    Writer.write(filename, properties.map(_.toPLTLMup));

  def writeOptimisticPLTLFile(property : LtlFormula, filename : String) : Unit =
    writePTLTFile(optimistic(property), filename);

  def writePessimisticPLTLFile(property : LtlFormula, filename : String) : Unit =
    writePTLTFile(pessimistic(property), filename);

  def check(solver : Solver, property : LtlFormula, logBasename : Option[String]) : ModelCheckerResult = {
    def writeSolverLog(logFilename : Option[String], solverInstance: SolverInstance) : Unit =
      logFilename.foreach(log => Writer.write(log, solverInstance.insights.flatMap(_.explain)));

    val optimisticSolverInstance = solver.create(optimistic(property), logBasename.map(_ + "_opt.log"));
    val optimisticResult = optimisticSolverInstance.check();
    writeSolverLog(logBasename.map(_ + "_opt.txt"), optimisticSolverInstance);
    if(optimisticResult == SATISFIABLE) NOT_SATISFIED;
    else{
      if(optimisticResult == UNSATISFIABLE && optimisticSolverInstance.insights.forall(!_.dependOnMaybe))
        SATISFIED;
      else{
        val pessimisticSolverInstance = solver.create(pessimistic(property), logBasename.map(_ + "_pes.log"));
        val pessimisticResult = pessimisticSolverInstance.check();
        writeSolverLog(logBasename.map(_ + "_pes.txt"), pessimisticSolverInstance);
        if(pessimisticResult == UNSATISFIABLE) SATISFIED;
        else if (optimisticResult.errorFound || pessimisticResult.errorFound) VERIFICATION_ERROR;
        else POSSIBLY_SATISFIED;
      }
    }
  }

  def toXML : Seq[String] = {
    val header = "<gxl xmlns:xbel='www.cs.toronto.edu/xbel' xmlns:xlink='xlink'>";
    val graph = "\t<graph ID='" + name + "' edgemode='directed'>";
    val pks = states.flatMap(_.toXML(atomicFormulae) :+ "") ++ transitions.flatMap(_.toXML :+ "");
    val endGraph= "\t</graph>";
    val footer = "</gxl>";
    Seq(header, graph, "") ++ pks.map(l => "\t\t" + l) ++ Seq(endGraph, footer);
  }

  def writeXML(filename : String) : Unit = Writer.write(filename, toXML);

}

object PartialKripkeStructure {

  private def extractLiteral(node : Node) : Option[Literal] = {
    val name = node.attributes.asAttrMap("name");
    val value = node.attributes.asAttrMap("value");
    value match {
      case "T" => Some(AtomicFormula(name));
      case "F" => Some(!AtomicFormula(name));
      case "M" => None;
    }
  }

  private def extractNode(node : Node) : State = {
    val isInitial = node.attributes.asAttrMap.get("xbel:initial").exists(_.toLowerCase == "true");
    val name = node.attributes.asAttrMap("ID");
    val literals = (node \ "attr").flatMap(extractLiteral);
    State(name, isInitial, literals);
  }

  private def extractTransition(states : Seq[State])(node : Node) : Transition = {
    val attributes = node.attributes.asAttrMap;
    val stateMap = states.map(s => s.name -> s).toMap;
    val from = attributes("from");
    val to = attributes("to");
    Transition(stateMap(from), stateMap(to));
  }

  private def extractGraph(node : Node) : PartialKripkeStructure = {
    val states = (node \ "node").map(extractNode);
    val transitions = (node \ "edge").map(extractTransition(states));
    PartialKripkeStructure(node.attributes.asAttrMap("ID"), states.toList, transitions.toList);
  }

  def apply(filename : String) : Seq[PartialKripkeStructure] = {
    val document = XML.loadFile(filename);
    (document \ "graph").map(extractGraph);
  }

}