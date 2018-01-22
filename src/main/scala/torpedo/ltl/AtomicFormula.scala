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

sealed abstract class AtomicFormula extends Literal {

  def id : String;

  def positive : AugmentedAtomicFormula = AugmentedAtomicFormula(id, value = true);

  def negative : AugmentedAtomicFormula = AugmentedAtomicFormula(id, value = false);

  def toXML(value : String) : String = "<attr type='prop' name='" + id + "' value='" + value + "'/>";

  override def unary_! : Literal = NegatedAtomicFormula(this);

  override def toPLTLMup : String = id;

  override def toTRP : String = id;

  override def toSMV : String = id;

  override def atomicFormula : AtomicFormula = this;

  override def complementClosure : Set[Literal] = Set(positive, !negative);

  override def complementClosed : AugmentedAtomicFormula = positive;

  override def original: Literal = this;

  override def isPositive : Boolean = true;

}

case class AugmentedAtomicFormula(id : String, value : Boolean) extends AtomicFormula {

  override def toPLTLMup : String = id + (if(value) "_p" else "_n");

  override def toTRP: String = id + (if(value) "_p" else "_n");

  override def toSMV : String = id + (if(value) "_p" else "_n");

  override def complementClosed : AugmentedAtomicFormula = this;

  override def original: Literal =
    if(value) Atom(id);
    else !Atom(id);

}

case class Atom(id : String) extends AtomicFormula;

object AtomicFormula {

  def apply(symbol: Symbol) : AtomicFormula = Atom(symbol.name);

  def apply(id: String) : AtomicFormula = Atom(id);

}