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
package torpedo.ltl

import scala.util.parsing.combinator.RegexParsers

object LtlFormulaParser extends RegexParsers {

  private val literal : Parser[LtlFormula] = """[A-Za-z_][A-Za-z_0-9]*""".r.^^(id => AtomicFormula(id));

  private val constant : Parser[LtlFormula] =
    (not("True[A-Za-z0-9_]".r) ~> "True").^^(_ => True) |
    (not("False[A-Za-z0-9_]".r) ~> "False").^^(_ => False);

  private lazy val expression : Parser[LtlFormula] = equivalence;

  private val atom : Parser[LtlFormula] = "(" ~> expression <~ ")" | constant | literal;

  private val unary : Parser[LtlFormula] =
    not("X[A-Za-z0-9_]".r) ~ "X" ~> unary.^^(X) |
    not("F[A-Za-z0-9_]".r) ~ "F" ~> unary.^^(F) |
    not("G[A-Za-z0-9_]".r) ~ "G" ~> unary.^^(G) |
    ("~"|"!") ~> unary.^^(!_) | atom;

  private val temporal : Parser[LtlFormula] =
    ((unary <~ not("B[A-Za-z0-9_]".r) ~ "B") ~ temporal).^^(x => Before(x._1, x._2)) |
    ((unary <~ not("U[A-Za-z0-9_]".r) ~ "U") ~ temporal).^^(x => Until(x._1, x._2))  |
    ((unary <~ not("W[A-Za-z0-9_]".r) ~ "W") ~ temporal).^^(x => G(x._1) | Until(x._1, x._2))  |
      unary;

  private val conjunction : Parser[LtlFormula] = repsep(temporal, "&").^^(Conjunction(_).simplify);

  private val disjunction : Parser[LtlFormula] = repsep(conjunction, "|").^^(Disjunction(_).simplify);

  private val implication : Parser[LtlFormula] = repsep(disjunction, "->"|"=>").^^(_.reduceLeft((x,y) => x -> y));

  private val equivalence : Parser[LtlFormula] = repsep(implication, "<->"|"="|"<==>"|"<=>").^^(
    _.reduceLeft((x,y) => !x & !y | x & y));

  def parse(line : String) : LtlFormula = parseAll(expression, line).get;

}
