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

import torpedo.ltl.LtlFormula
import torpedo.main._
import com.microsoft.z3._
import torpedo.topologicalproof.{Clause, TPClause}
import torpedo.utilities.ProcessHandler

import scala.collection.mutable

class Z3(clauses : Seq[Clause], logFilename : Option[String],k: Int)
   extends SolverInstance {

  protected var result : SolverResult = UNKNOWN;

  protected var bound=k;

  private val formulae = clauses.map(_.clause);
  private val possibleTPClauses = clauses.map(_.tpClause).toArray;

  private var unsatCore = false;

  private var actualTPClauses : Seq[TPClause] = Seq[TPClause]();

  private var topologicalProofError : NoValue = NoError;

  check()

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


  override def input : Seq[String] = translate(formulae);

  protected def translate(formulae: Seq[LtlFormula]) :
  Seq[String] = formulae.map(_.toPLTLMup + "\n");

  override def check() : SolverResult = {
    val ctx=null;
    try {
      val ctx: Context = new Context(new java.util.HashMap[String, String])

      val indexes = 0 to bound by 1

      // Selectors
      val selectorsPropositions=indexes.map(s => (ctx.mkBoolConst("l"+s.toString))).toArray;

      val onlyOneSelectorTrue: BoolExpr=selectorsPropositions.map(s => s).reduce((A1,A2) =>ctx.mkOr(A1,A2));

      val selectorMutualExclusionTrue: BoolExpr=selectorsPropositions.map(
        s1 =>
          ctx.mkIff(s1,
          selectorsPropositions.filter(s2 => !s1.equals(s2)).map(s3 => ctx.mkNot(s3)).reduce((A1,A2) =>ctx.mkOr(A1,A2)))).reduce((A1,A2) =>ctx.mkAnd(A1,A2));

      // InLoop
      val inLoopPropositions=indexes.map(s => ctx.mkBoolConst("inLoop"+s.toString)).toArray;

      val inLoopExpr: BoolExpr=
        ctx.mkAnd(
            indexes.filter(index => index>0).map(index =>
                        ctx.mkIff(inLoopPropositions(index),
                              ctx.mkOr(inLoopPropositions(index-1),
                                    selectorsPropositions(index)))).reduce((A1,A2) =>ctx.mkAnd(A1,A2)),
          ctx.mkIff(inLoopPropositions(0),selectorsPropositions(0))
        );

      // LoopExists
      val loopExists: BoolExpr=ctx.mkIff(ctx.mkBoolConst("loopExists"),selectorsPropositions.map(s => s).reduce((A1,A2) =>ctx.mkOr(A1,A2)));

      // Formula

      val a: BoolExpr = ctx.mkBoolConst("a")
      val b: BoolExpr = ctx.mkBoolConst("b")
      val c: BoolExpr = ctx.mkBoolConst("c")
      val cn: BoolExpr = ctx.mkNot(ctx.mkBoolConst("c"))
      val formula: BoolExpr = ctx.mkAnd(ctx.mkAnd(onlyOneSelectorTrue,selectorMutualExclusionTrue,inLoopExpr,loopExists))

      val solver: com.microsoft.z3.Solver = ctx.mkSolver()   // Get a solver from the Context object
      val status: Status=solver.check(formula,cn,c,a,b)                                    // Add the formula to the solver (BoolExpr)

      if(status== Status.UNSATISFIABLE)                 // Check if the formula is satisfiable or not
      {
        val core: Array[BoolExpr] = solver.getUnsatCore

        println(core)   // Result is true
      }
    }
      catch {
        case e: Throwable => println("Got some other kind of exception")
         println(e)
      }

    result};

}

object Z3 extends Solver{

  override def check(clauses: Seq[Clause], k: Int) : SolverResult = new Z3(clauses, None,k).check();

  override def check(clauses: Seq[Clause], logFilename : Option[String], k: Int) : SolverResult =
    new Z3(clauses, logFilename,k).check();


  override def create(clauses: Seq[Clause], logFilename : Option[String], k: Int) = new Z3(clauses, logFilename,k);
}

