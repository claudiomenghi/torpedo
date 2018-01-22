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

abstract class LtlFormula(protected val priority : Int) {

  def unary_! : LtlFormula = Negation(this);

  def &(rhs : LtlFormula) : LtlFormula = Conjunction(this :: rhs :: Nil).simplify;

  def |(rhs : LtlFormula) : LtlFormula = Disjunction(this :: rhs :: Nil).simplify;

  def U(rhs : LtlFormula) : LtlFormula = Until(this, rhs);

  def ->(rhs : LtlFormula) : LtlFormula = Implication(this, rhs);

  def toPLTLMup : String;

  def toTRP : String;

  def toSMV : String;

  def toNNF : LtlFormula;

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

  override def toPLTLMup : String = "~" + formula.enclose(_.toPLTLMup, priority);

  override def toTRP : String = "~(" + formula.toTRP + ")";

  override def toSMV : String = "!(" + formula.toSMV + ")";

  override def complementClosed : LtlFormula = throw new IllegalArgumentException("Formula not in NNF!");

  override def toNNF : LtlFormula =
    formula match {
      case Negation(f) => f.toNNF;
      case Conjunction(formulae) => Disjunction(formulae.map(!_)).toNNF;
      case Disjunction(formulae) => Conjunction(formulae.map(!_)).toNNF;
      case Implication(lhs, rhs) => (lhs & !rhs).toNNF;
      case Before(lhs, rhs) => Until(!lhs, rhs).toNNF;
      case Release(lhs, rhs) => Until(!lhs, !rhs).toNNF;
      case Until(lhs, rhs) => Release(!lhs, !rhs).toNNF;
      case X(f) => X(!f).toNNF;
      case G(f) => F(!f).toNNF;
      case F(f) => G(!f).toNNF;
      case True => False;
      case False => True;
    }

}

case class Conjunction(formulae : Seq[LtlFormula]) extends LtlFormula(10){

  override def &(rhs : LtlFormula) : LtlFormula = Conjunction(formulae :+ rhs);

  override def toPLTLMup : String = formulae.map(_.enclose(_.toPLTLMup, priority)).mkString(" & ");

  override def toTRP : String = formulae.map("(" + _.toTRP + ")").mkString(" & ");

  override def toSMV : String = formulae.map("(" + _.toSMV + ")").mkString(" & ");

  override def toNNF : LtlFormula = Conjunction(formulae.map(_.toNNF));

  override def complementClosed : LtlFormula = Conjunction(formulae.map(_.complementClosed));

  override def equals(o: scala.Any): Boolean =
    o match {
      case Conjunction(f) => formulae.toSet == f.toSet;
      case _ => false;
    }

  override def hashCode(): Int = formulae.toSet.hashCode();

  def simplify : LtlFormula =
    formulae.size match {
      case 0 => True;
      case 1 => formulae.head;
      case _ => this;
    }

}

case class Disjunction(formulae : Seq[LtlFormula]) extends LtlFormula(11){

  override def equals(o: scala.Any): Boolean =
    o match {
      case Disjunction(f) => formulae.toSet == f.toSet;
      case _ => false;
    }

  override def hashCode(): Int = formulae.toSet.hashCode();

  override def |(rhs : LtlFormula) : LtlFormula = Disjunction(formulae :+ rhs);

  override def toPLTLMup : String = formulae.map(_.enclose(_.toPLTLMup, priority)).mkString(" | ");

  override def toTRP : String = formulae.map("(" + _.toTRP + ")").mkString(" | ");

  override def toSMV : String = formulae.map("(" + _.toSMV + ")").mkString(" | ");

  override def toNNF : LtlFormula = Disjunction(formulae.map(_.toNNF));

  override def complementClosed : LtlFormula = Disjunction(formulae.map(_.complementClosed));

  def simplify : LtlFormula =
    formulae.size match {
      case 0 => False;
      case 1 => formulae.head;
      case _ => this;
    }

}

case class Implication(lhs : LtlFormula, rhs : LtlFormula) extends LtlFormula(12){

  override def toPLTLMup : String = lhs.enclose(_.toPLTLMup, priority) + " => " + rhs.enclose(_.toPLTLMup, priority);

  override def toTRP : String = "(" + lhs.toTRP + ") => (" + rhs.toTRP + ")";

  override def toSMV : String = "(" + lhs.toSMV + ") -> (" + rhs.toSMV + ")";

  override def toNNF : LtlFormula = (!lhs).toNNF | rhs.toNNF;

  override def complementClosed : LtlFormula = throw new IllegalArgumentException("Formula not in NNF!");

}

case class Before(lhs : LtlFormula, rhs : LtlFormula) extends LtlFormula(5){

  override def toPLTLMup : String = lhs.enclose(_.toPLTLMup, priority - 1) + " B " + rhs.enclose(_.toPLTLMup, priority);

  override def toTRP : String = (!Until(!lhs, rhs)).toTRP;

  override def toSMV : String = Release(lhs, !rhs).toSMV;

  override def toNNF : LtlFormula = Release(lhs, !rhs).toNNF;

  override def complementClosed : LtlFormula = throw new IllegalArgumentException("Formula not in NNF!");

}

case class Release(lhs : LtlFormula, rhs : LtlFormula) extends LtlFormula(5){

  override def toPLTLMup : String = Before(lhs, !rhs).toPLTLMup;

  override def toTRP : String = (!Until(!lhs, !rhs)).toTRP;

  override def toSMV : String = "(" + lhs.toSMV + ") V (" + rhs.toSMV + ")";

  override def toNNF : LtlFormula = Release(lhs.toNNF, rhs.toNNF);

  override def complementClosed : LtlFormula = Release(lhs.complementClosed, rhs.complementClosed);

}

case class Until(lhs : LtlFormula, rhs : LtlFormula) extends LtlFormula(5){

  override def toPLTLMup : String = lhs.enclose(_.toPLTLMup, priority - 1) + " U " + rhs.enclose(_.toPLTLMup, priority);

  override def toTRP : String = "(" + lhs.toTRP + ") until (" + rhs.toTRP + ")";

  override def toSMV : String = "(" + lhs.toSMV + ") U (" + rhs.toSMV + ")";

  override def toNNF : LtlFormula = Until(lhs.toNNF, rhs.toNNF);

  override def complementClosed : LtlFormula = Until(lhs.complementClosed, rhs.complementClosed);

}

case class X(formula: LtlFormula) extends LtlFormula(0){

  override def toPLTLMup : String = formula.enclose("X", _.toPLTLMup, priority);

  override def toTRP : String = "next(" + formula.toTRP + ")";

  override def toSMV : String = "X(" + formula.toSMV + ")";

  override def toNNF : LtlFormula = X(formula.toNNF);

  override def complementClosed : LtlFormula = X(formula.complementClosed);

}

case class G(formula: LtlFormula) extends LtlFormula(0){

  override def toPLTLMup : String = formula.enclose("G", _.toPLTLMup, priority);

  override def toTRP : String = "always(" + formula.toTRP + ")";

  override def toSMV : String = "G(" + formula.toSMV + ")";

  override def toNNF : LtlFormula = Release(False, formula).toNNF;

  override def complementClosed : LtlFormula = throw new IllegalArgumentException("Formula not in NNF!");

}

case class F(formula: LtlFormula) extends LtlFormula(0){

  override def toPLTLMup : String = formula.enclose("F", _.toPLTLMup, priority);

  override def toTRP : String = "sometime(" + formula.toTRP + ")";

  override def toSMV : String = "F(" + formula.toSMV + ")";

  override def toNNF : LtlFormula = Until(True, formula).toNNF;

  override def complementClosed : LtlFormula = throw new IllegalArgumentException("Formula not in NNF!");

}

case object True extends LtlFormula(0){

  override def toPLTLMup : String = "True";

  override def toTRP : String = "True";

  override def toSMV : String = "TRUE";

  override def toNNF : LtlFormula = this;

  override def complementClosed : LtlFormula = this;

}

case object False extends LtlFormula(0){

  override def toPLTLMup : String = "False";

  override def toTRP : String = "False";

  override def toSMV : String = "FALSE";

  override def toNNF : LtlFormula = this;

  override def complementClosed : LtlFormula = this;

}