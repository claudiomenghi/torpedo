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
package thrive.insights

import thrive.ltl.Literal
import thrive.pks.{PartialKripkeStructure, State, Transition}

class Slicer(pks : PartialKripkeStructure) {

  private var states : Set[State] = Set();

  private var transitions : Seq[Transition] = Seq();

  private var clearInitialStates = true;

  private var stateLiterals : Map[State, Set[Literal]] = Map();

  private def process(state : State) : State = {
    val result = state.restrict(stateLiterals(state));
    if(clearInitialStates)
      result.clearInitial;
    else
      result;
  }

  def keepProposition(state : State, literals : Set[Literal]) : Unit = {
    if(stateLiterals.contains(state)) {
      val newLiterals = stateLiterals(state) ++ literals;
      stateLiterals = stateLiterals + (state -> newLiterals);
    }
    stateLiterals = stateLiterals + (state -> literals);
    states = states + state;
  }

  def keepTransition(state : State, next : Seq[State]) : Unit = {
    next.map(Transition(state, _)).foreach(t => transitions = transitions :+ t);
    states = states + state ++ next;
  }

  def keepInitialStates() : Unit = {
    clearInitialStates = false;
    states = states ++ pks.states.filter(_.isInitial);
  }

  def slice() : PartialKripkeStructure = {
    val s = states.map(process);
    val t = transitions.map(t => Transition(process(t.from), process(t.to)));
    new PartialKripkeStructure(pks.name, s.toList, t.toList);
  }

}
