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
import thrive.pks.State

case class StatePredicate(state : State, literals : Set[Literal], override val dependOnMaybe : Boolean) extends Insight {

  private def literalText : String =
    if(literals.size == 1) literals.head.toPLTLMup;
    else literals.map(_.toPLTLMup).mkString("{", ", ", "}");

  override def explain : Option[String] = Some(literalText + " (" + state.name + ")");

}
