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

import java.io.FileNotFoundException

import org.xml.sax.SAXParseException
import thrive.ltl._
import thrive.mc._
import thrive.pks.encoders.LTLEncoder
import thrive.solver.{SATISFIABLE, Solver, SolverInstance, UNSATISFIABLE}
import thrive.utilities.Writer

import scala.xml.{Node, XML}

case class PartialKripkeStructure(name : String, states : List[State], transitions : List[Transition]) {

  private final val ENABLE_OPTIMIZATION : Boolean = false;

  require(states.forall(s => transitions.exists(_.from == s)), "There is a state without outgoing transitions!");

  private[pks] val atomicFormulae : Set[AtomicFormula] = states.flatMap(_.atomicFormulae).toSet;

  private def writeSolverInputFile(solverInstance: SolverInstance, filename : String) : Unit =
    Writer.write(filename, solverInstance.input);

  def check(solver : Solver, property : LtlFormula, solverInputPrefix : Option[String],
            solverLogPrefix : Option[String], outputPrefix : Option[String]) : ModelCheckerResult = {

    def writeSolverLog(logFilename : Option[String], solverInstance: SolverInstance) : Unit =
      logFilename.foreach(log => Writer.write(log, solverInstance.insights.flatMap(_.explain)));

    val encoder = LTLEncoder(this);
    val optimisticSolverInstance = solver.create(encoder.optimistic(property), solverLogPrefix.map(_ + "_opt.log"));
    solverInputPrefix.foreach(prefix => writeSolverInputFile(optimisticSolverInstance, prefix));
    val optimisticResult = optimisticSolverInstance.check();
    writeSolverLog(outputPrefix.map(_ + "_opt.txt"), optimisticSolverInstance);
    if(optimisticResult == SATISFIABLE) NOT_SATISFIED;
    else{
      if(ENABLE_OPTIMIZATION &&
          optimisticResult == UNSATISFIABLE &&
          optimisticSolverInstance.insights.forall(!_.dependOnMaybe))
        SATISFIED;
      else{
        val pessimisticSolverInstance =
          solver.create(encoder.pessimistic(property), solverLogPrefix.map(_ + "_pes.log"));
        solverInputPrefix.foreach(prefix => writeSolverInputFile(pessimisticSolverInstance, prefix));
        val pessimisticResult = pessimisticSolverInstance.check();
        writeSolverLog(outputPrefix.map(_ + "_pes.txt"), pessimisticSolverInstance);
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
      case "TT" => Some(AtomicFormula(name));
      case "T" => Some(AtomicFormula(name));
      case "FF" => Some(!AtomicFormula(name));
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

  private def extractGraph(node : Node) : Option[PartialKripkeStructure] = {
    val states = (node \ "node").map(extractNode);
    val transitions = (node \ "edge").map(extractTransition(states));
    try {
      Some(PartialKripkeStructure(node.attributes.asAttrMap("ID"), states.toList, transitions.toList));
    }
    catch {
      case _ : SAXParseException => None;
    }
  }

  def apply(filename : String) : Seq[PartialKripkeStructure] = {
    try {
      val document = XML.loadFile(filename);
      (document \ "graph").flatMap(extractGraph);
    }
    catch {
      case _ : FileNotFoundException => Seq();
      case _ : SAXParseException => Seq();
    }
  }

}