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
package thrive.main

import java.io.FileNotFoundException

import thrive.ltl._
import thrive.pks.PartialKripkeStructure
import thrive.solver.{HybridPLTLMup, PLTLMup, Solver}

import scala.io.Source

object Main {

  def readProperty(filename : String) : Option[LtlFormula] = {
    try {
      val clauses = Source.fromFile(filename).getLines().map(LtlFormulaParser.parse).toSeq;
      Some(Conjunction(clauses).simplify);
    }
    catch {
      case _ : FileNotFoundException => None;
    }
  }

  def writeSolverInput(pks : PartialKripkeStructure, property : LtlFormula, options: Options) : Unit = {
    options.solverInput.foreach(prefix => pks.writeOptimisticSolverInput(options.solver, property, prefix));
    options.solverInput.foreach(prefix => pks.writePessimisticSolverInput(options.solver, property, prefix));
  }

  def checkProperty(ksFilename : String, propertyFilename : String, options: Options) : Unit = {
    val ks = PartialKripkeStructure(ksFilename);
    val property = readProperty(propertyFilename);
    if(ks.isEmpty)
      println("Input/output error on PKS!");
    else if(property.isEmpty)
      println("Input/output error on property!");
    else {
      writeSolverInput(ks.head, property.get, options);
      println(ks.head.check(options.solver, property.get, options.solverLog, options.output));
    }
  }

  def checkProperty(name : String, property : LtlFormula, ks : PartialKripkeStructure, solver : Solver) : Unit = {
    println("Checking property " + name + ":");
    println("    " + property.toPLTLMup);
    ks.writeOptimisticSolverInput(PLTLMup, property, name + "_opt.pltl");
    ks.writePessimisticSolverInput(PLTLMup, property, name + "_pes.pltl");
    println("    " + ks.check(solver, property, Some(name), Some(name)));
    println("");
  }

  def main(args: Array[String]): Unit = {
    if(args.length < 2){
      println("Usage: thrive [options] <PKS XML file> <property file>");
    }
    else{
      val files = args.takeRight(2);
      val options = DefaultOptions;
      if(options.processCommandLineArguments(args.dropRight(2).toList))
        checkProperty(files.head, files(1), options);
    }
  }

}
