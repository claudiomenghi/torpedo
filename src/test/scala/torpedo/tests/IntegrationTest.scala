/**
  * Copyright (C) 2018  Alessandro M. Rizzi <alessandromaria.rizzi@polimi.it>
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
package torpedo.tests

import torpedo.ltl.LtlFormulaParser
import torpedo.main.Success
import torpedo.mc.{NOT_SATISFIED, NuSMV, POSSIBLY_SATISFIED, SATISFIED}
import torpedo.pks.PartialKripkeStructure
import torpedo.solver.HybridPLTLMup

import scala.io.Source

class IntegrationTest extends UnitTest {

  private def checkProperty(pksFile : String, propertyFile : String) = {
    val phi = getClass.getResource(propertyFile).getPath;
    val pks = getClass.getResource(pksFile).getPath;

    val p = Source.fromFile(phi).getLines().map(LtlFormulaParser.parse).toSeq.head.get;
    for {
      ks <- PartialKripkeStructure(pks);
      result <- ks.check(HybridPLTLMup, NuSMV, p, None, None, None, None, None)
    }
      yield result;
  }

  "Torpedo" should "verify a property on a PKS" in{
    checkProperty("/callee/callee-1.xml", "/callee/phi1.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/callee/callee-1.xml", "/callee/phi2.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/callee/callee-1.xml", "/callee/phi3.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/callee/callee-1.xml", "/callee/phi4.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/callee/callee-1.xml", "/callee/phi5.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/callee/callee-2.xml", "/callee/phi1.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/callee/callee-2.xml", "/callee/phi2.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/callee/callee-2.xml", "/callee/phi3.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/callee/callee-2.xml", "/callee/phi4.pltl") should be (Success(SATISFIED));
    checkProperty("/callee/callee-2.xml", "/callee/phi5.pltl") should be (Success(NOT_SATISFIED));
    checkProperty("/callee/callee-3.xml", "/callee/phi1.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/callee/callee-3.xml", "/callee/phi2.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/callee/callee-3.xml", "/callee/phi3.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/callee/callee-3.xml", "/callee/phi4.pltl") should be (Success(SATISFIED));
    checkProperty("/callee/callee-3.xml", "/callee/phi5.pltl") should be (Success(NOT_SATISFIED));
    checkProperty("/callee/callee-4.xml", "/callee/phi1.pltl") should be (Success(NOT_SATISFIED));
    checkProperty("/callee/callee-4.xml", "/callee/phi2.pltl") should be (Success(NOT_SATISFIED));
    checkProperty("/callee/callee-4.xml", "/callee/phi3.pltl") should be (Success(SATISFIED));
    checkProperty("/callee/callee-4.xml", "/callee/phi4.pltl") should be (Success(SATISFIED));
    checkProperty("/callee/callee-4.xml", "/callee/phi5.pltl") should be (Success(NOT_SATISFIED));

    checkProperty("/caller/caller-1.xml", "/caller/psi1.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/caller/caller-1.xml", "/caller/psi2.pltl") should be (Success(NOT_SATISFIED));
    checkProperty("/caller/caller-1.xml", "/caller/psi3.pltl") should be (Success(SATISFIED));
    checkProperty("/caller/caller-1.xml", "/caller/psi4.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/caller/caller-1.xml", "/caller/psi5.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/caller/caller-2.xml", "/caller/psi1.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/caller/caller-2.xml", "/caller/psi2.pltl") should be (Success(NOT_SATISFIED));
    checkProperty("/caller/caller-2.xml", "/caller/psi3.pltl") should be (Success(SATISFIED));
    checkProperty("/caller/caller-2.xml", "/caller/psi4.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/caller/caller-2.xml", "/caller/psi5.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/caller/caller-3.xml", "/caller/psi1.pltl") should be (Success(SATISFIED));
    checkProperty("/caller/caller-3.xml", "/caller/psi2.pltl") should be (Success(SATISFIED));
    checkProperty("/caller/caller-3.xml", "/caller/psi3.pltl") should be (Success(SATISFIED));
    checkProperty("/caller/caller-3.xml", "/caller/psi4.pltl") should be (Success(SATISFIED));
    checkProperty("/caller/caller-3.xml", "/caller/psi5.pltl") should be (Success(SATISFIED));
    checkProperty("/caller/caller-4.xml", "/caller/psi1.pltl") should be (Success(SATISFIED));
    checkProperty("/caller/caller-4.xml", "/caller/psi2.pltl") should be (Success(NOT_SATISFIED));
    checkProperty("/caller/caller-4.xml", "/caller/psi3.pltl") should be (Success(SATISFIED));
    checkProperty("/caller/caller-4.xml", "/caller/psi4.pltl") should be (Success(SATISFIED));
    checkProperty("/caller/caller-4.xml", "/caller/psi5.pltl") should be (Success(SATISFIED));

    checkProperty("/caller-callee/caller-callee-1.xml", "/caller-callee/eta1.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/caller-callee/caller-callee-1.xml", "/caller-callee/eta2.pltl") should be (Success(SATISFIED));
    checkProperty("/caller-callee/caller-callee-1.xml", "/caller-callee/eta3.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/caller-callee/caller-callee-1.xml", "/caller-callee/eta4.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/caller-callee/caller-callee-1.xml", "/caller-callee/eta5.pltl") should be (Success(NOT_SATISFIED));
    checkProperty("/caller-callee/caller-callee-2.xml", "/caller-callee/eta1.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/caller-callee/caller-callee-2.xml", "/caller-callee/eta2.pltl") should be (Success(SATISFIED));
    checkProperty("/caller-callee/caller-callee-2.xml", "/caller-callee/eta3.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/caller-callee/caller-callee-2.xml", "/caller-callee/eta4.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/caller-callee/caller-callee-2.xml", "/caller-callee/eta5.pltl") should be (Success(NOT_SATISFIED));
    checkProperty("/caller-callee/caller-callee-3.xml", "/caller-callee/eta1.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/caller-callee/caller-callee-3.xml", "/caller-callee/eta2.pltl") should be (Success(SATISFIED));
    checkProperty("/caller-callee/caller-callee-3.xml", "/caller-callee/eta3.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/caller-callee/caller-callee-3.xml", "/caller-callee/eta4.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/caller-callee/caller-callee-3.xml", "/caller-callee/eta5.pltl") should be (Success(POSSIBLY_SATISFIED));
    checkProperty("/caller-callee/caller-callee-4.xml", "/caller-callee/eta1.pltl") should be (Success(NOT_SATISFIED));
    checkProperty("/caller-callee/caller-callee-4.xml", "/caller-callee/eta2.pltl") should be (Success(NOT_SATISFIED));
    checkProperty("/caller-callee/caller-callee-4.xml", "/caller-callee/eta3.pltl") should be (Success(NOT_SATISFIED));
    checkProperty("/caller-callee/caller-callee-4.xml", "/caller-callee/eta4.pltl") should be (Success(SATISFIED));
    checkProperty("/caller-callee/caller-callee-4.xml", "/caller-callee/eta5.pltl") should be (Success(NOT_SATISFIED));


  }

}
