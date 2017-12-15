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

import thrive.ltl.{AtomicFormula, LtlFormula}
import thrive.pks.{PartialKripkeStructure, State}

abstract class Encoder[T] (pks: PartialKripkeStructure) {

  protected val p : Map[State, AtomicFormula] =
    Array.range(0, pks.states.size).map(i => pks.states(i) -> AtomicFormula("s" + i)).toMap;

  protected val initialStates : Set[State] =
    pks.states.filter(_.isInitial).toSet;

  protected val transitionMap : Map[State, List[State]] =
    pks.transitions.groupBy(_.from).map(x => x._1 -> x._2.map(_.to));

  require(pks.states.toSet == transitionMap.keySet, "There is a state without outgoing transitions!");

  def optimistic(property : LtlFormula) : Seq[T];

  def pessimistic(property : LtlFormula) : Seq[T];

}
