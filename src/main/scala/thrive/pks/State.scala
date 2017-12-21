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

import thrive.ltl._

case class State(name : String, isInitial : Boolean, literals : Seq[Literal], maybe : Set[AtomicFormula]) {

  def atomicFormulae : Set[AtomicFormula] = literals.map(_.atomicFormula).toSet ++ maybe;

  def ->(next : State) : Transition = Transition(this, next);

  def predicate : LtlFormula = Conjunction(literals).simplify;

  def approximation(af : Set[AtomicFormula], f : AtomicFormula => Literal) : Seq[(Literal, Boolean)] = {
    val assignedLiterals = literals.flatMap(_.complementClosure).toSet;
    val missingLiterals = af.flatMap(_.complementClosure.map(_.atomicFormula)) -- assignedLiterals.map(_.atomicFormula);
    (assignedLiterals.map((_, false)) ++ missingLiterals.map(f).map((_, true))).toSeq;
  }

  private def literalsToXML(atomicFormulae : Set[AtomicFormula]) : Seq[String] = {
    def literalValue(literal: Literal) : String =
      literal match {
        case _: NegatedAtomicFormula => "F";
        case _ => "T";
      }

    val tf = literals.map(l => l.atomicFormula.toXML(literalValue(l)));
    val maybe = atomicFormulae.diff(literals.map(_.atomicFormula).toSet).map(_.atomicFormula.toXML("M"));
    tf ++ maybe;
  }

  private def initialXML : String = if(isInitial) " xbel:initial='true'" else "";

  def toXML(atomicFormulae : Set[AtomicFormula]) : Seq[String] =
    ("<node ID='" + name + "'" + initialXML + ">") +: literalsToXML(atomicFormulae).map(l => "\t" + l) :+ "</node>";

}

object State{

  def apply(id : Symbol, isInitial : Boolean, atoms : Literal*) : State = new State(id.name, isInitial, atoms, Set());

  def apply(id : Symbol, atoms : Literal*) : State = new State(id.name, isInitial = false, atoms, Set());

}
