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
package thrive.pks.encoders

import thrive.ltl.{AtomicFormula, Literal, LtlFormula}
import thrive.pks.{PartialKripkeStructure, State}

case class SMVEncoder(pks: PartialKripkeStructure) extends Encoder[String](pks) {

  private def stateSequence(states : Seq[State]) =
    if(states.size == 1)
      p(states.head).id;
    else
      states.map(p(_).id).mkString("{", ", ", "}");

  private def stateTransition(state: State) =
    "state = " + state.name + " : " + stateSequence(transitionMap(state)) + ";";

  private def stateAtomicFormulae(state : State, f : AtomicFormula => Literal) : Seq[(String, String)] =
    state.approximation(pks.atomicFormulae, f).filter(_._1.isPositive).map(c => (c._1.atomicFormula.id, p(state).id));




  private def variables : Seq[String] = {
    val stateVariables : String = Array.range(0, pks.states.size).map("s" + _).mkString("state : {", ",", "};");

    val propositionVariables =
      pks.atomicFormulae.flatMap(x => Seq(x.id + "_n", x.id + "_p")).map(x => x + " : boolean;").toSeq;

    "VAR" +: (stateVariables +: propositionVariables).map("\t" + _);
  }

  private def assign(f : AtomicFormula => Literal) : Seq[String] = {
    val initialState = "init(state) := " + stateSequence(initialStates.toSeq) + ";";
    val nextState = "next(state) := case" +: pks.states.map(stateTransition).map("\t" + _) :+ "\tesac;";
    val states = initialState +: nextState;

    val propositionState = pks.states.flatMap(stateAtomicFormulae(_, f)).groupBy(_._1).map(x => x._1 -> x._2.map(_._2));
    val propositions = propositionState.map(y => y._1 + " := state in " + y._2.mkString("{", ", ", "}") + ";");

    "ASSIGN" +: (states ++ propositions).map("\t" + _);
  }

  private def toSVM(f : AtomicFormula => Literal) = "MODULE main" +: (variables ++ assign(f)).map("\t" + _);


  override def optimistic(property: LtlFormula) : Seq[String] = toSVM(p => p);

  override def pessimistic(property: LtlFormula) : Seq[String] = toSVM(p => !p);

}
