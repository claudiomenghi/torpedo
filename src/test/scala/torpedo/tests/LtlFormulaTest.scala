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

import torpedo.ltl._

class LtlFormulaTest extends UnitTest {

  private val x = Atom("x");

  private val y = Atom("y");


  "A LtlFormula" should "contain an LTL formula" in {
    x&y should be (Conjunction(Seq(x, y)));
    x|y should be (Disjunction(Seq(x, y)));
    x->y should be (Implication(x,y));
    x U y should be (Until(x,y));
    !(x U True) should be (Negation(Until(x,True)));
    !x should be (NegatedAtomicFormula(x));
    !NegatedAtomicFormula(x) should be (Atom("x"));
    X(G(F(False))) should be(X(G(F(False))));
    Before(x, y) should be(Before(x, y));
    Release(x, y) should be(Release(x, y));
  }

  it should "provide PLTL-MUP input conversion" in {
    x.toPLTLMup should be ("x");
    (x&y).toPLTLMup should be ("x & y");
    (x|y).toPLTLMup should be ("x | y");
    (x->y).toPLTLMup should be ("x => y");
    (x U y).toPLTLMup should be ("x U y");
    (!(x U True)).toPLTLMup should be ("~(x U True)");
    (!x).toPLTLMup should be ("~x");
    X(G(F(False))).toPLTLMup should be("X G F False");
    Before(x, y).toPLTLMup should be("x B y");
    Release(x, y).toPLTLMup should be("x B ~y");
  }

  it should "provide TRP input conversion" in {
    x.toTRP should be ("x");
    (x&y).toTRP should be ("(x) & (y)");
    (x|y).toTRP should be ("(x) | (y)");
    (x->y).toTRP should be ("(x) => (y)");
    (x U y).toTRP should be ("(x) until (y)");
    (!(x U True)).toTRP should be ("~((x) until (True))");
    (!x).toTRP should be ("~ (x)");
    X(G(F(False))).toTRP should be("next(always(sometime(False)))");
    Before(x, y).toTRP should be("~((~ (x)) until (y))");
    Release(x, y).toTRP should be("~((~ (x)) until (~ (y)))");
  }

  it should "provide SMV input conversion" in {
    x.toSMV should be ("x");
    (x&y).toSMV should be ("(x) & (y)");
    (x|y).toSMV should be ("(x) | (y)");
    (x->y).toSMV should be ("(x) -> (y)");
    (x U y).toSMV should be ("(x) U (y)");
    (!(x U True)).toSMV should be ("!((x) U (TRUE))");
    (!x).toSMV should be ("!(x)");
    X(G(F(False))).toSMV should be("X(G(F(FALSE)))");
    Before(x, y).toSMV should be("(x) V (!(y))");
    Release(x, y).toSMV should be("(x) V (y)");
  }

  it should "provide NNF input conversion" in {
    x.toNNF should be (x);
    (x&y).toNNF should be (x&y);
    (x|y).toNNF should be (x|y);
    (x->y).toNNF should be (!x | y);
    (x U y).toNNF should be (x U y);
    (!(x U True)).toNNF should be (Release(!x, False));
    (!x).toNNF should be (!x);
    X(G(F(False))).toNNF should be(X(Release(False, True U False)));
    (!X(G(F(False)))).toNNF should be(X(True U Release(False, True)));
    Before(x, y).toNNF should be(Release(x, !y));
    Release(x, y).toNNF should be(Release(x, y));
  }

  it should "provide complement closed formula" in {
    x.complementClosed should be (x.positive);
    (x&y).complementClosed should be (x.positive&y.positive);
    (x|y).complementClosed should be (x.positive|y.positive);
    (x->y).toNNF.complementClosed should be (x.negative | y.positive);
    (x U y).complementClosed should be (x.positive U y.positive);
    (!(x U True)).toNNF.complementClosed should be (Release(x.negative, False));
    (!x).complementClosed should be (x.negative);
    X(G(F(False))).toNNF.complementClosed should be(X(Release(False, True U False)));
    //(!X(G(F(False)))).complementClosed should be(X(True U Release(False, True)));
    Before(x, y).toNNF.complementClosed should be(Release(x.positive, y.negative));
    Release(x, y).toNNF.complementClosed should be(Release(x.positive, y.positive));
  }



}
