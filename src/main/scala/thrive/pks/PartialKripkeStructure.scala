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
import thrive.insights.{Clause, Insight, Slicer}
import thrive.ltl._
import thrive.mc._
import thrive.pks.encoders.{LTLEncoder, SMVEncoder}
import thrive.solver.Solver
import thrive.utilities.Writer

import scala.xml.{Node, XML}

case class PartialKripkeStructure(name : String, states : List[State], transitions : List[Transition]) {

  private[pks] val atomicFormulae : Set[AtomicFormula] = states.flatMap(_.atomicFormulae).toSet;

  private def writeInputFile(input : Seq[String], filename : String) : Unit = Writer.write(filename, input);

  private def proveProperty(result : ModelCheckerResult, property : LtlFormula, solver : Solver,
                            inputPrefix : Option[String], logPrefix : Option[String]) : Seq[Insight] = {
    def retrieveInsights(model : Seq[Clause]) : Seq[Insight] = {
      val solverInstance = solver.create(model, logPrefix.map(_ + "_solver.log"));
      inputPrefix.foreach(prefix => writeInputFile(solverInstance.input, prefix + "_solver.in"));
      solverInstance.check();
      solverInstance.insights;
    }

    result match {
      case SATISFIED => retrieveInsights(LTLEncoder(this).pessimistic(property));
      case POSSIBLY_SATISFIED => retrieveInsights(LTLEncoder(this).optimistic(property));
      case _ => Seq();
    }
  }

  private def buildSlice(insights : Seq[Insight], outputFilename : String) : Unit = {
    val slicer = new Slicer(this);
    insights.foreach(_.computeSlice(slicer));
    val pks = slicer.slice();
    pks.writeXML(outputFilename);
  }

  private def checkProperty(mc : ModelChecker, property : LtlFormula, inputPrefix : Option[String],
                            logPrefix : Option[String], traceFilename : Option[String]) : ModelCheckerResult = {
    val encoder = SMVEncoder(this);
    def writeTrace(modelCheckerInstance: ModelCheckerInstance) : Unit =
      traceFilename.foreach(Writer.write(_, encoder.trace(modelCheckerInstance.counterexample).output));

    val optimisticModelCheckerInstance = mc.create(encoder.optimistic(property), logPrefix.map(_ + "_mc_opt.log"));
    inputPrefix.foreach(prefix => writeInputFile(optimisticModelCheckerInstance.input, prefix + "_mc_opt.in"));
    val optimisticResult = optimisticModelCheckerInstance.check();
    if(optimisticResult == NOT_SATISFIED) {
      writeTrace(optimisticModelCheckerInstance);
      NOT_SATISFIED;
    }
    else{
      val pessimisticModelChecker = mc.create(encoder.pessimistic(property), logPrefix.map(_ + "_mc_pes.log"));
      inputPrefix.foreach(prefix => writeInputFile(pessimisticModelChecker.input, prefix + "_mc_pes.in"));
      val pessimisticResult = pessimisticModelChecker.check();
      if(pessimisticResult == SATISFIED) SATISFIED;
      else if (optimisticResult.errorFound || pessimisticResult.errorFound) VERIFICATION_ERROR;
      else {
        writeTrace(pessimisticModelChecker);
        POSSIBLY_SATISFIED;
      }
    }
  }

  private val transitionMap : Map[String, List[String]] =
    transitions.groupBy(_.from).map(x => x._1.name -> x._2.map(_.to.name));

  private def checkPredicates(slice : PartialKripkeStructure) : Boolean = {
    val requiredPredicates = slice.states.map(x => (x.name, x.literals.toSet, x.maybe));
    val predicateMap = states.map(s => s.name -> (s.literals.toSet, s.maybe)).toMap;
    requiredPredicates.forall( t =>
      predicateMap.contains(t._1) && t._2.subsetOf(predicateMap(t._1)._1) && t._3.subsetOf(predicateMap(t._1)._2));
  }

  def recheckNeeded(slice : PartialKripkeStructure) : Boolean = {
    val sliceInitial = slice.states.filter(_.isInitial);
    if(sliceInitial.nonEmpty && sliceInitial.map(_.name).toSet != states.filter(_.isInitial).map(_.name).toSet)
      true;
    else if (!slice.transitionMap.forall(pair => transitionMap(pair._1) == pair._2))
      true;
    else if (!checkPredicates(slice))
      true;
    else
      false;
  }

  def check(solver : Solver, mc : ModelChecker, property : LtlFormula,
            inputPrefix : Option[String], logPrefix : Option[String],
            traceFilename : Option[String], output : Option[String], slice : Option[String]) : ModelCheckerResult = {
    val result = checkProperty(mc, property, inputPrefix, logPrefix, traceFilename);
    lazy val insights = proveProperty(result, property, solver, inputPrefix, logPrefix);
    output.foreach(Writer.write(_, insights.flatMap(_.explain)));
    slice.foreach(buildSlice(insights, _));
    result;
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

  private def extractMaybe(node : Node) : Option[AtomicFormula] = {
    val name = node.attributes.asAttrMap("name");
    val value = node.attributes.asAttrMap("value");
    value match {
      case "M" => Some(AtomicFormula(name));
      case _ => None;
    }
  }

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
    val maybe = (node \ "attr").flatMap(extractMaybe);
    State(name, isInitial, literals, maybe.toSet);
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