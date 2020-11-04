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

import torpedo.topologicalproof.Clause

trait Solver {

  def check(formulae : Seq[Clause], k: Int) : SolverResult;

  def check(clause : Clause, k: Int) : SolverResult = check(Seq(clause),k);

  def check(clauses : Seq[Clause], logFilename : Option[String], k: Int) : SolverResult;

  def create(clauses: Seq[Clause], logFilename : Option[String], k: Int): SolverInstance;

}
