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
import thrive.insights._
import thrive.ltl._
import thrive.pks.{PartialKripkeStructure, State}

case class LTLEncoder(pks: PartialKripkeStructure) extends Encoder[Clause](pks) {

  private final val USE_SLOW_STATE_PREDICATE : Boolean = true;

  private def distinct : Seq[Clause] = {
    val sp = p.values.toArray;
    val c = Array.range(0, sp.length - 1).map(i => sp(i) -> !Disjunction(Array.range(i+1, sp.length).map(sp)).simplify);
    (c :+ Disjunction(sp)).map(G).map(Clause(_, PKSConstraint));
  }

  private def transitionPredicate(from : State, to : Seq[State]) = G(p(from) -> X(Disjunction(to.map(p)).simplify));

  private def transitionPredicate : Seq[Clause] =
    transitionMap.map(t => Clause(transitionPredicate(t._1, t._2), StateTransition(t._1, t._2))).toSeq;

  private def initialPredicate : Clause =
    Clause(Disjunction(initialStates.toSeq.map(s => p(s))).simplify, InitialState(initialStates.toSeq));

  private def fastStatePredicate(f : AtomicFormula => Literal) : Seq[Clause] =
    pks.states.map { state =>
      val (literals, dependOnMaybe) = state.approximation(pks.atomicFormulae, f).unzip;
      val insight = StatePredicate(state, literals.map(_.original).toSet, dependOnMaybe.exists(x => x));
      Clause(G(p(state) -> Conjunction(literals).simplify), insight);
    }

  private def slowStatePredicate(f : AtomicFormula => Literal) : Seq[Clause] =
    for{
      s <- pks.states;
      (lit, maybeDependent) <- s.approximation(pks.atomicFormulae, f)
    }
      yield Clause(G(p(s) -> lit), StatePredicate(s, Set(lit.original), maybeDependent));

  private def statePredicate(f : AtomicFormula => Literal) : Seq[Clause] =
    if(USE_SLOW_STATE_PREDICATE)
      slowStatePredicate(f);
    else
      fastStatePredicate(f);

  private def transformProperty(property : LtlFormula) : Clause = Clause(!property.complementClosed, Property);

  override def optimistic(property : LtlFormula) : Seq[Clause] =
    distinct ++ transitionPredicate ++ statePredicate(a => a) ++ Seq(initialPredicate, transformProperty(property));

  override def pessimistic(property : LtlFormula) : Seq[Clause] =
    distinct ++ transitionPredicate ++ statePredicate(a => !a) ++ Seq(initialPredicate, transformProperty(property));

}
