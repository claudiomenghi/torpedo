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

import thrive.ltl.{AtomicFormula, F, G, LtlFormula}
import thrive.pks.{PartialKripkeStructure, State}
import thrive.solver.{PLTLMup, Solver}

object Main {

  def checkProperty(name : String, property : LtlFormula, ks : PartialKripkeStructure, solver : Solver) : Unit = {
    println("Checking property " + name + ":");
    println("    " + property.toPLTLMup);
    ks.writeOptimisticPLTLFile(property, name + "_opt.pltl");
    ks.writePessimisticPLTLFile(property, name + "_pes.pltl");
    println("    " + ks.check(solver, property, name));
    println("");
  }

  def main(args: Array[String]): Unit = {
    val g = AtomicFormula('g);
    val r = AtomicFormula('r);
    val s0 = State('s0, !g, r);
    val s1 = State('s1, g, !r);
    val s2 = State('s2);
    val ks = PartialKripkeStructure(List(s0, s1, s2), List(s0 -> s1, s0 -> s2, s1 -> s0, s2 -> s0), Set(s0));
    val phi1 = G(F(r));
    val phi2 = G(F(g));
    val phi3 = G(r -> G(g));

    val solver = PLTLMup;
    checkProperty("phi1", phi1, ks, solver);
    checkProperty("phi2", phi2, ks, solver);
    checkProperty("phi3", phi3, ks, solver);
  }

}
