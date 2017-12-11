package thrive.ltl

import scala.util.parsing.combinator.RegexParsers

object LtlFormulaParser extends RegexParsers {

  private val literal : Parser[LtlFormula] = """[A-Za-z_][A-Za-z_0-9]*""".r.^^(id => AtomicFormula(id));

  private val constant : Parser[LtlFormula] =
    (not("True[A-Za-z0-9_]".r) ~> "True").^^(_ => True) |
    (not("False[A-Za-z0-9_]".r) ~> "False").^^(_ => False);

  private lazy val expression : Parser[LtlFormula] = implication;

  private val atom : Parser[LtlFormula] = "(" ~> expression <~ ")" | constant | literal;

  private val unary : Parser[LtlFormula] =
    not("X[A-Za-z0-9_]".r) ~ "X" ~> unary.^^(X) |
    not("F[A-Za-z0-9_]".r) ~ "F" ~> unary.^^(F) |
    not("G[A-Za-z0-9_]".r) ~ "G" ~> unary.^^(G) |
    "~" ~> unary.^^(!_) | atom;

  private val temporal : Parser[LtlFormula] =
    ((unary <~ not("B[A-Za-z0-9_]".r) ~ "B") ~ temporal).^^(x => Before(x._1, x._2)) |
    ((unary <~ not("U[A-Za-z0-9_]".r) ~ "U") ~ temporal).^^(x => Until(x._1, x._2))  |
      unary;

  private val conjunction : Parser[LtlFormula] = repsep(temporal, "&").^^(Conjunction(_).simplify);

  private val disjunction : Parser[LtlFormula] = repsep(conjunction, "|").^^(Disjunction(_).simplify);

  private val implication : Parser[LtlFormula] = repsep(disjunction, "->").^^(_.reduceLeft((x,y) => x -> y));

  def parse(line : String) : LtlFormula = parseAll(expression, line).get;

}
