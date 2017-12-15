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
package thrive.utilities

import java.io.{InputStream, OutputStream, OutputStreamWriter}

import scala.io.Source
import scala.sys.process.{Process, ProcessIO}

abstract class ProcessHandler(logFilename : Option[String]) {

  private var alreadyComputed = false;

  protected val SUCCESS = 0;

  protected def command : String;

  protected def input : Seq[String];

  protected def processLine(line : String) : Unit;

  private def processOutput(inputStream: InputStream) : Unit = {
    val lines = Source.fromInputStream(inputStream).getLines.toSeq;
    lines.foreach(processLine);
    logFilename.foreach(Writer.write(_, lines));
    inputStream.close();
  }

  private def processError(inputStream: InputStream) : Unit = {
    inputStream.close();
  }

  private def processInput(outputStream : OutputStream) : Unit = {
    val writer = new OutputStreamWriter(outputStream);
    input.foreach(writer.write);
    writer.close();
  }

  private val process = Process(command);
  private val io = new ProcessIO(processInput, processOutput, processError);

  protected def exitValue() : Int = process.run(io).exitValue();

  protected def computeOrRetrieve[T](result : => T, failure : T) : T = {
    if(!alreadyComputed && exitValue() != SUCCESS) {
      alreadyComputed = true;
      failure;
    }
    else {
      alreadyComputed = true;
      result;
    }
  }



}
