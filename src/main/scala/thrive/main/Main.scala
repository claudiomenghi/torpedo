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
import thrive.solver.{HybridPLTLMup, Solver}

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

  def checkProperty(ksFilename : String, propertyFilename : String, solver : Solver) : Unit = {
    val ks = PartialKripkeStructure(ksFilename);
    val property = readProperty(propertyFilename);
    if(ks.isEmpty)
      println("Input/output error on PKS!");
    else if(property.isEmpty)
      println("Input/output error on property!");
    else
      println(ks.head.check(solver, property.get, None));
  }

  def checkProperty(name : String, property : LtlFormula, ks : PartialKripkeStructure, solver : Solver) : Unit = {
    println("Checking property " + name + ":");
    println("    " + property.toPLTLMup);
    ks.writeOptimisticPLTLFile(property, name + "_opt.pltl");
    ks.writePessimisticPLTLFile(property, name + "_pes.pltl");
    println("    " + ks.check(solver, property, Some(name)));
    println("");
  }

  def peterson() : Unit = {
    val ks = PartialKripkeStructure("examples/peterson.xml").head;

    val solver = HybridPLTLMup;
    checkProperty("p1", G(F(AtomicFormula('y))), ks, solver);
  }

  def semaphore(): Unit = {
    val g = AtomicFormula('g);
    val r = AtomicFormula('r);
    val ks = PartialKripkeStructure("examples/semaphore.xml").head;

    val phi1 = G(F(r));
    val phi2 = G(F(g));
    val phi3 = G(r -> G(g));

    val solver = HybridPLTLMup;
    checkProperty("phi1", phi1, ks, solver);
    checkProperty("phi2", phi2, ks, solver);
    checkProperty("phi3", phi3, ks, solver);
  }

  def phone() : Unit = {
    val connected = AtomicFormula('CONNECTED);
    val offhook = AtomicFormula('OFFHOOK);
    val solver = HybridPLTLMup;
    val ks = PartialKripkeStructure("examples/caller-comp2.xml").head;
    checkProperty("psi1", G(connected -> X(!offhook)), ks, solver);
    checkProperty("psi2", G(connected -> X(!offhook -> !connected)), ks, solver);
  }

  def main(args: Array[String]): Unit = {
    semaphore();
    phone();
    peterson();
  }

}
