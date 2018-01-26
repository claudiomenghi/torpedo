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
import torpedo.ltl._
import torpedo.main.{ProofFailure, Result, Success}

class HybridPLTLMup(clauses : Seq[Clause], logFilename : Option[String]) extends PLTLMup(clauses, logFilename) {

  override protected def command = "docker run --rm -i torpedoframework/pltl-mup-hybrid";

  override protected def translate(formulae: Seq[LtlFormula]) : Seq[String] =
    Seq(formulae.map("(" + _.toTRP + ")").mkString(" & "));

  override protected def exitValue() : Int = {
    val returnCode = super.exitValue();
    if (result == SATISFIABLE)
      SUCCESS;
    else
      returnCode;
  }

  override protected def extractTopologicalProofClause(line : String) : Result[Option[TPClause]] = {
    if(line.startsWith("MUS Size:"))
      Success(None);
    else {
      val formula = LtlFormulaParser.parse(line.split(":")(1).trim);
      val topologicalProof = clauses.map(clause => clause.clause.toTRP -> clause.tpClause).toMap;
      formula match {
        case Some(f) => Success(topologicalProof.get(f.toTRP));
        case None => ProofFailure;
      }
    }
  }

}

object HybridPLTLMup extends Solver{

  override def check(clauses: Seq[Clause]) : SolverResult = new HybridPLTLMup(clauses, None).check();

  override def check(clauses: Seq[Clause], logFilename : Option[String]) : SolverResult =
    new HybridPLTLMup(clauses, logFilename).check();

  override def create(clauses: Seq[Clause], logFilename: Option[String]) = new HybridPLTLMup(clauses, logFilename);
}

