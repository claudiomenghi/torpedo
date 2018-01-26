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
package torpedo.solver

import torpedo.topologicalproof.{Clause, TPClause}
import torpedo.ltl.LtlFormula
import torpedo.main._
import torpedo.utilities.ProcessHandler

class PLTLMup(clauses : Seq[Clause], logFilename : Option[String])
  extends ProcessHandler(logFilename) with SolverInstance {

  override protected def command = "docker run --rm -i torpedoframework/pltl-mup";

  protected var result : SolverResult = UNKNOWN;

  private val formulae = clauses.map(_.clause);
  private val possibleTPClauses = clauses.map(_.tpClause).toArray;

  private var unsatCore = false;

  private var actualTPClauses : Seq[TPClause] = Seq[TPClause]();

  private var topologicalProofError : NoValue = NoError;

  override def topologicalProof : Result[Seq[TPClause]] =
    if(check() == ERROR) ProofFailure;
    else topologicalProofError.map(_ => actualTPClauses);

  private def extractClauseIndex(line : String) : Option[Int] = {
    val value = line.split(":").headOption;
    try {
      val index = value.map(_.toInt);
      index.filter(i => i >= 0 && i < possibleTPClauses.length);
    } catch {
      case _ : NumberFormatException => None;
    }
  }

  protected def extractTopologicalProofClause(line : String) : Result[Option[TPClause]] =
    Success(extractClauseIndex(line).map(possibleTPClauses).filterNot(actualTPClauses.contains));

  override protected def processLine(line : String) : Unit = {
    if(line.startsWith("Satisfiable"))
      result = SATISFIABLE;
    if(line.startsWith("Unsatisfiable"))
      result = UNSATISFIABLE;

    if(unsatCore) {
      extractTopologicalProofClause(line) match {
        case Success(None) => unsatCore = false;
        case Success(Some(i)) => actualTPClauses = actualTPClauses :+ i;
        case error : Failure => topologicalProofError = error;
        case _ => topologicalProofError = ProofFailure;
      }
    }

    if(line.startsWith("Minimal unsatisfiable subset:"))
      unsatCore = true;
  }

  override def input : Seq[String] = translate(formulae);

  protected def translate(formulae: Seq[LtlFormula]) : Seq[String] = formulae.map(_.toPLTLMup + "\n");

  override def check() : SolverResult = computeOrRetrieve(result, ERROR);

}

object PLTLMup extends Solver{

  override def check(clauses: Seq[Clause]) : SolverResult = new PLTLMup(clauses, None).check();

  override def check(clauses: Seq[Clause], logFilename : Option[String]) : SolverResult =
    new PLTLMup(clauses, logFilename).check();

  override def create(clauses: Seq[Clause], logFilename : Option[String]) = new PLTLMup(clauses, logFilename);
}
