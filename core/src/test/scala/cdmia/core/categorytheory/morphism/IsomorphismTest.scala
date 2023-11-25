package cdmia.core.categorytheory.morphism

import cdmia.core.categorytheory.Object
import org.scalatest.funsuite.*

class IsomorphismTest extends AnyFunSuite {
  val a = new Object("a")
  val b = new Object("b")
  val mab = new Morphism("mab", a, b)
  val mba = new Morphism("mba", b, a)
  val maa = new Morphism("maa", a, a)
  val mbb = new Morphism("m4", b, b)

  test("An Isomorphism should be correctly built") {
    assertResult(true)(new Isomorphism(mab, mba).isInstanceOf[Isomorphism])
    assertResult(true)(new Isomorphism(mba, mab).isInstanceOf[Isomorphism])
  }

  test("An Isomorphism should not be built if the morphisms are not inverse") {
    assertThrows[IllegalArgumentException](new Isomorphism(mab, maa))
    assertThrows[IllegalArgumentException](new Isomorphism(mbb, mba))
  }
}
