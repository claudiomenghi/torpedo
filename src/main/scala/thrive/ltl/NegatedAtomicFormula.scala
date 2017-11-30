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

case class NegatedAtomicFormula(atom : AtomicFormula) extends Literal {

  override def unary_! : Literal = atom;

  override def toString: String = "not " + atom.toString;

  override def toPLTLMup : String = "~" + atom.toPLTLMup;

  override def atomicFormula : AtomicFormula = atom;

  override def complementClosure : Set[Literal] = atom.complementClosure.map(!_);

  override def complementClosed : AugmentedAtomicFormula = atom.negative;

  override def original: Literal = !atom.original;

}
