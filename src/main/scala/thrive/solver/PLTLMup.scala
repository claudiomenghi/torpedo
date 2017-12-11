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
package thrive.solver

import java.io.{InputStream, OutputStream, OutputStreamWriter}

import thrive.insights.{Clause, Insight}
import thrive.ltl.LtlFormula
import thrive.utilities.Writer

import scala.io.Source
import scala.sys.process.{Process, ProcessIO}

class PLTLMup(clauses : Seq[Clause], logFilename : Option[String]) extends SolverInstance {

  protected val COMMAND = "docker run -i pltl-mup";
  protected val SUCCESS = 0;

  protected var result : SolverResult = UNKNOWN;
  private val process = Process(COMMAND);
  private val io = new ProcessIO(processInput, processOutput, processError);

  private val formulae = clauses.map(_.clause);
  private val possibleInsights = clauses.map(_.insight).toArray;

  private var unsatCore = false;

  private var actualInsights : Seq[Insight] = Seq[Insight]();

  private var alreadyChecked = false;

  override def insights : Seq[Insight] = actualInsights;

  private def extractClauseIndex(line : String) : Option[Int] = {
    val value = line.split(":").headOption;
    try {
      val index = value.map(_.toInt);
      index.filter(i => i >= 0 && i < possibleInsights.length);
    } catch {
      case _ : NumberFormatException => None;
    }
  }

  protected def extractInsight(line : String) : Option[Insight] =
    extractClauseIndex(line).map(possibleInsights).filterNot(actualInsights.contains);

  private def processLine(line : String) : Unit = {
    if(line.startsWith("Satisfiable"))
      result = SATISFIABLE;
    if(line.startsWith("Unsatisfiable"))
      result = UNSATISFIABLE;

    if(unsatCore) {
      val insight = extractInsight(line);
      insight match {
        case None => unsatCore = false;
        case Some(i) => actualInsights = actualInsights :+ i;
      }
    }

    if(line.startsWith("Minimal unsatisfiable subset:"))
      unsatCore = true;
  }

  protected def translate(formulae: Seq[LtlFormula]) : Seq[String] = formulae.map(_.toPLTLMup + "\n");

  private def processInput(outputStream : OutputStream) : Unit = {
    val writer = new OutputStreamWriter(outputStream);
    translate(formulae).foreach(writer.write);
    writer.close();
  }

  private def processOutput(inputStream: InputStream) : Unit = {
    val lines = Source.fromInputStream(inputStream).getLines.toSeq;
    lines.foreach(processLine);
    logFilename.foreach(Writer.write(_, lines));
  }

  protected def processError(inputStream: InputStream) : Unit = {
    val lines = Source.fromInputStream(inputStream).getLines;
    lines.foreach(println);
  }

  protected def exitValue() : Int = process.run(io).exitValue();

  def check() : SolverResult = {
    if(!alreadyChecked && exitValue() != SUCCESS) {
      alreadyChecked = true;
      ERROR;
    }
    else {
      alreadyChecked = true;
      result;
    }
  }

}

object PLTLMup extends Solver{

  override def check(clauses: Seq[Clause]) : SolverResult = new PLTLMup(clauses, None).check();

  override def check(clauses: Seq[Clause], logFilename : String) : SolverResult =
    new PLTLMup(clauses, Some(logFilename)).check();

  override def create(clauses: Seq[Clause], logFilename: String) = new PLTLMup(clauses, Some(logFilename));
}
