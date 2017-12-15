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
package thrive.mc

import thrive.utilities.ProcessHandler

class NuSMV(smv : Seq[String], logFilename : Option[String])
  extends ProcessHandler(logFilename) with ModelCheckerInstance {

  private var result : ModelCheckerResult = VERIFICATION_ERROR;

  private var trace : Seq[String] = Seq();

  private var loopStart : Int = 0;

  override protected def command: String = "docker run -i nusmv";

  override def input : Seq[String] = smv;

  override def check() : ModelCheckerResult = computeOrRetrieve(result, VERIFICATION_ERROR);

  private def isPropertySatisfied(line: String) = {
    val parts = line.split(" ");
    parts.take(2).sameElements(Array("--","specification")) && parts.takeRight(2).sameElements(Array("is","true"));
  }

  private def isPropertyNotSatisfied(line: String) = {
    val parts = line.split(" ");
    parts.take(2).sameElements(Array("--","specification")) && parts.takeRight(2).sameElements(Array("is","false"));
  }

  override protected def processLine(line: String): Unit = {
    if(isPropertySatisfied(line))
      result = SATISFIED;
    if(isPropertyNotSatisfied(line))
      result = NOT_SATISFIED;
    val cl = line.split(" ").mkString("")
    if(cl == "--Loopstartshere")
      loopStart = trace.size;
    if(cl.startsWith("state=s"))
      trace = trace :+ cl.split("=").last;
  }

}

object NuSMV extends ModelChecker {
  override def create(input: Seq[String], logFilename: Option[String]) : NuSMV = new NuSMV(input, logFilename);
}
