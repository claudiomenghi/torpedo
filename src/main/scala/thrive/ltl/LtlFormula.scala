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
package thrive.ltl

abstract class LtlFormula(protected val priority : Int) {

  def unary_! : LtlFormula = Negation(this);

  def &(rhs : LtlFormula) : LtlFormula = Conjunction(this :: rhs :: Nil).simplify;

  def |(rhs : LtlFormula) : LtlFormula = Disjunction(this :: rhs :: Nil).simplify;

  def U(rhs : LtlFormula) : LtlFormula = Until(this, rhs);

  def ->(rhs : LtlFormula) : LtlFormula = Implication(this, rhs);

  def toPLTLMup : String;

  def complementClosed : LtlFormula;

  def enclose(f : LtlFormula => String, nextPriority : Int) : String =
    if(nextPriority < priority)
      "(" + f(this) + ")";
    else f(this);

  def enclose(prefix : String, f : LtlFormula => String, nextPriority : Int) : String =
    if(nextPriority < priority)
      prefix + "(" + f(this) + ")";
    else prefix + " " + f(this);


}

case class Negation(formula: LtlFormula) extends LtlFormula(0){

  override def toString: String = "not(" + formula.toString + ")";

  override def toPLTLMup : String = "~" + formula.enclose(_.toPLTLMup, priority);

  override def complementClosed : LtlFormula =
    formula match {
      case Conjunction(formulae) => Disjunction(formulae.map(!_)).complementClosed;
      case Disjunction(formulae) => Conjunction(formulae.map(!_)).complementClosed;
      case Implication(lhs, rhs) => (lhs & !rhs).complementClosed;
      case Before(lhs, rhs) => Until(!lhs, rhs).complementClosed;
      case Until(lhs, rhs) => Before(!lhs, rhs).complementClosed;
      case X(f) => X(!f).complementClosed;
      case G(f) => F(!f).complementClosed;
      case F(f) => G(!f).complementClosed;
      case True => False;
      case False => True;
    }

}

case class Conjunction(formulae : Seq[LtlFormula]) extends LtlFormula(10){

  override def toString: String = formulae.map("(" + _ + ")").mkString(" & ");

  override def &(rhs : LtlFormula) : LtlFormula = Conjunction(formulae :+ rhs);

  override def toPLTLMup : String = formulae.map(_.enclose(_.toPLTLMup, priority)).mkString(" & ");

  def simplify : LtlFormula =
    formulae.size match {
      case 0 => True;
      case 1 => formulae.head;
      case _ => this;
    }

  override def complementClosed : LtlFormula = Conjunction(formulae.map(_.complementClosed));

}

case class Disjunction(formulae : Seq[LtlFormula]) extends LtlFormula(11){

  override def toString: String = formulae.map("(" + _ + ")").mkString(" | ");

  override def |(rhs : LtlFormula) : LtlFormula = Disjunction(formulae :+ rhs);

  override def toPLTLMup : String = formulae.map(_.enclose(_.toPLTLMup, priority)).mkString(" | ");

  def simplify : LtlFormula =
    formulae.size match {
      case 0 => False;
      case 1 => formulae.head;
      case _ => this;
    }

  override def complementClosed : LtlFormula = Disjunction(formulae.map(_.complementClosed));

}

case class Implication(lhs : LtlFormula, rhs : LtlFormula) extends LtlFormula(12){
  override def toString: String = "(" + lhs + ") -> (" + rhs + ")";

  override def toPLTLMup : String = lhs.enclose(_.toPLTLMup, priority) + " => " + rhs.enclose(_.toPLTLMup, priority);

  override def complementClosed : LtlFormula = (!lhs).complementClosed | rhs.complementClosed;
}

case class Before(lhs : LtlFormula, rhs : LtlFormula) extends LtlFormula(5){
  override def toString: String = "(" + lhs.toString + ") before (" + rhs.toString + ")";

  override def toPLTLMup : String = lhs.enclose(_.toPLTLMup, priority - 1) + " B " + rhs.enclose(_.toPLTLMup, priority);

  override def complementClosed : LtlFormula = Before(lhs.complementClosed, rhs.complementClosed);
}

case class Until(lhs : LtlFormula, rhs : LtlFormula) extends LtlFormula(5){
  override def toString: String = "(" + lhs.toString + ") until (" + rhs.toString + ")";

  override def toPLTLMup : String = lhs.enclose(_.toPLTLMup, priority - 1) + " U " + rhs.enclose(_.toPLTLMup, priority);

  override def complementClosed : LtlFormula = Until(lhs.complementClosed, rhs.complementClosed);
}

case class X(formula: LtlFormula) extends LtlFormula(0){
  override def toString: String = "next(" + formula.toString + ")";

  override def toPLTLMup : String = formula.enclose("X", _.toPLTLMup, priority);

  override def complementClosed : LtlFormula = X(formula.complementClosed);
}

case class G(formula: LtlFormula) extends LtlFormula(0){
  override def toString: String = "always(" + formula.toString + ")";

  override def toPLTLMup : String = formula.enclose("G", _.toPLTLMup, priority);

  override def complementClosed : LtlFormula = G(formula.complementClosed);
}

case class F(formula: LtlFormula) extends LtlFormula(0){
  override def toString: String = "sometime(" + formula.toString + ")";

  override def toPLTLMup : String = formula.enclose("F", _.toPLTLMup, priority);

  override def complementClosed : LtlFormula = F(formula.complementClosed);
}

case object True extends LtlFormula(0){
  override def toString : String = "True";

  override def toPLTLMup : String = "True";

  override def complementClosed : LtlFormula = this;
}

case object False extends LtlFormula(0){
  override def toString : String = "False";

  override def toPLTLMup : String = "False";

  override def complementClosed : LtlFormula = this;
}