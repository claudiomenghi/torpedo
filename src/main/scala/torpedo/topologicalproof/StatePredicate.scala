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
package torpedo.topologicalproof

import torpedo.ltl.Literal
import torpedo.pks.State

case class StatePredicate(state : State, literals : Set[Literal], override val dependOnMaybe : Boolean)
  extends TPClause {

  private def getLiteral(literal: Literal) : String = {
    if(state.maybe.contains(literal.atomicFormula))
      "?" + literal.atomicFormula.toPLTLMup;
    else
      literal.toPLTLMup;
  }

  private def literalText : String =
    if(literals.size == 1) getLiteral(literals.head);
    else literals.map(getLiteral).mkString("{", ", ", "}");

  override def explain : Option[String] = Some(literalText + " (" + state.name + ")");

  override def computeSlice(slicer: Slicer): Unit = slicer.keepProposition(state, literals);

}
