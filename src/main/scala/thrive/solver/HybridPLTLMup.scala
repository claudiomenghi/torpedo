package thrive.solver

import java.io.InputStream

import thrive.insights.{Clause, Insight}
import thrive.ltl._

class HybridPLTLMup(clauses : Seq[Clause], logFilename : Option[String]) extends PLTLMup(clauses, logFilename) {

  override protected val COMMAND = "docker run -i pltl-mup-hybrid";

  override protected def translate(formulae: Seq[LtlFormula]) : Seq[String] =
    Seq(formulae.map("(" + _.toTRP + ")").mkString(" & "));

  override protected def exitValue() : Int = {
    val returnCode = super.exitValue();
    if (result == SATISFIABLE)
      SUCCESS;
    else
      returnCode;
  }

  def normalized(formula: LtlFormula) : LtlFormula =
    formula match {
      case G(f) => Before(False, normalized(!f))
      case F(f) => Until(True, normalized(f))
      case True => True;
      case False => False;
      case Conjunction(formulae) => Conjunction(formulae.map(normalized)).simplify;
      case Disjunction(formulae) => Disjunction(formulae.map(normalized)).simplify;
      case Negation(Negation(f)) => normalized(f);
      case Negation(Conjunction(f)) => normalized(Disjunction(f.map(!_)));
      case Negation(Disjunction(f)) => normalized(Conjunction(f.map(!_)));
      case Negation(Implication(lhs, rhs)) => normalized(lhs & !rhs);
      case Negation(Before(lhs, rhs)) => normalized(Until(!lhs, rhs));
      case Negation(Until(lhs, rhs)) => normalized(Before(!lhs, rhs));
      case Negation(X(f)) => normalized(X(!f));
      case Negation(G(f)) => normalized(F(!f));
      case Negation(F(f)) => normalized(G(!f));
      case Negation(True) => False;
      case Negation(False) => True;
      case X(f) => X(normalized(f));
      case NegatedAtomicFormula(atom) => !normalized(atom);
      case a : AugmentedAtomicFormula => AtomicFormula(a.toPLTLMup);
      case a : AtomicFormula => a;
    }

  override protected def extractInsight(line : String) : Option[Insight] = {
    val formula = LtlFormulaParser.parse(line.split(":")(1).trim);
    val insights = clauses.map(clause => normalized(clause.clause) -> clause.insight).toMap;
    insights.get(formula);
  }

  override protected def processError(inputStream: InputStream) : Unit = {}

}

object HybridPLTLMup extends Solver{

  override def check(clauses: Seq[Clause]) : SolverResult = new HybridPLTLMup(clauses, None).check();

  override def check(clauses: Seq[Clause], logFilename : String) : SolverResult =
    new HybridPLTLMup(clauses, Some(logFilename)).check();

  override def create(clauses: Seq[Clause], logFilename: String) = new HybridPLTLMup(clauses, Some(logFilename));
}

