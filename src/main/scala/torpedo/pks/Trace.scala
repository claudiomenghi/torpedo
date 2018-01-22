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
package torpedo.pks

case class Trace(trace : Seq[State], loopStart : Option[Int]) {
  loopStart.foreach(l => require(l >= 0 && l < trace.size));

  private def loopJump : Option[Int] = loopStart.map(ls => Math.min(ls + 1, trace.size - 1));

  private def loop : Seq[String] = loopJump.map(l => trace.size + " : GOTO " + l).toSeq;

  def output : Seq[String] = trace.zipWithIndex.map(x => x._2 + " : " + x._1.name) ++ loop;

}
