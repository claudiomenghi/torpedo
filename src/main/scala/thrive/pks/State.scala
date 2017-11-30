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
package thrive.pks

import thrive.ltl.{AtomicFormula, Conjunction, Literal, LtlFormula}

case class State(name : String, literals : Seq[Literal]) {

  def atomicFormulae : Set[AtomicFormula] = literals.map(_.atomicFormula).toSet;

  def ->(next : State) : Transition = Transition(this, next);

  def predicate : LtlFormula = Conjunction(literals).simplify;

  def approximation(af : Set[AtomicFormula], f : AtomicFormula => Literal) : Seq[(Literal, Boolean)] = {
    val assignedLiterals = literals.flatMap(_.complementClosure).toSet;
    val missingLiterals = af.flatMap(_.complementClosure.map(_.atomicFormula)) -- assignedLiterals.map(_.atomicFormula);
    (assignedLiterals.map((_, false)) ++ missingLiterals.map(f).map((_, true))).toSeq;
  }

}

object State{

  def apply(id : Symbol, atoms : Literal*) : State = State(id.name, atoms);

}
