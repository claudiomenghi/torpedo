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
package torpedo.main

import java.io.FileNotFoundException

import torpedo.ltl._
import torpedo.mc.ModelCheckerResult
import torpedo.pks.PartialKripkeStructure

import scala.io.Source

object Main {

  private def readProperty(filename : String) : Result[LtlFormula] = {
    try {
      val clauses = Source.fromFile(filename).getLines().map(LtlFormulaParser.parse).toSeq;
      if(clauses.contains(None))
        InvalidFileFailure(filename);
      else
        Success(Conjunction(clauses.flatten).simplify);
    }
    catch {
      case _ : FileNotFoundException => FileNotFoundFailure(filename);
    }
  }

  private def checkProperty(pksFile : String, propertyFile : String, opt: Options) : Result[ModelCheckerResult] = {
    for{
      ks <- PartialKripkeStructure(pksFile);
      property <- readProperty(propertyFile);
      result <- ks.check(opt.solver, opt.modelChecker, property, opt.input, opt.log, opt.trace, opt.output, opt.slice)
    }
      yield result;
  }

  def analysis(args: Array[String]): Unit = {
    if(args.length < 2){
      println("Usage: torpedo analysis [options] <PKS XML file> <property file>");
    }
    else{
      val files = args.takeRight(2);
      val options = DefaultOptions;
      if(options.processCommandLineArguments(args.dropRight(2).toList))
        println(checkProperty(files.head, files(1), options));
    }
  }

  private def recheck(pksFilename : String, sliceFilename : String) : Result[Boolean] = {
    for {
      pks <- PartialKripkeStructure(pksFilename);
      slice <- PartialKripkeStructure(sliceFilename)
    }
      yield pks.recheckNeeded(slice);
  }

  def recheck(args: Array[String]) : Unit = {
    def asString(result : Boolean) : String =
      if(result) "Run analysis again!";
      else "Recheck confirmed analysis result!"

    if(args.length != 2)
      println("Usage: torpedo recheck <PKS XML file> <Slice XML file>");
    else
      println(recheck(args(0), args(1)).map(asString));
  }

  def main(args: Array[String]): Unit = {
    val currentDirectory = new java.io.File(".").getCanonicalPath
    if(args.length < 1)
      println("Usage: torpedo <command> [options]");
    else
      args.head match {
        case "analysis" => analysis(args.tail);
        case "recheck" => recheck(args.tail);
        case _ => println("Command not recognized!");
      }
  }

}
