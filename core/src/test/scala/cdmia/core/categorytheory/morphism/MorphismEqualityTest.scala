package cdmia.core.categorytheory.morphism

import cdmia.core.categorytheory.Object
import org.scalatest.funsuite.*

class MorphismEqualityTest extends AnyFunSuite {
  val a = new Object("a")
  val b = new Object("b")
  val m1 = new Morphism("m1", a, b)
  val m2 = new Morphism("m2", a, b)
  val m3 = new Morphism("m3", a, b)
  val m4 = new Morphism("m4", a, a)
  val m5 = new Morphism("m5", b, b)

  test("A MorphismEquality should be correctly built") {
    assertResult(true)(new MorphismEquality(List[Morphism](m1, m2)).isInstanceOf[MorphismEquality])
    assertResult(true)(new MorphismEquality(List[Morphism](m1, m2, m3)).isInstanceOf[MorphismEquality])
  }

  test("A MorphismEquality should not be built with less than two different morphism") {
    assertThrows[IllegalArgumentException](new MorphismEquality(List[Morphism]()))
    assertThrows[IllegalArgumentException](new MorphismEquality(List[Morphism](m1)))
  }

  test("A MorphismEquality should not be built with duplicate morphism") {
    assertThrows[IllegalArgumentException](new MorphismEquality(List[Morphism](m1, m2, m1)))
    assertThrows[IllegalArgumentException](new MorphismEquality(List[Morphism](m1, m1)))
    assertThrows[IllegalArgumentException](new MorphismEquality(List[Morphism](m1, m2, m3, m3)))
  }

  test("A MorphismEquality should not be built with morphisms that do not have the same domain and codomain") {
    assertThrows[IllegalArgumentException](new MorphismEquality(List[Morphism](m1, m2, m4)))
    assertThrows[IllegalArgumentException](new MorphismEquality(List[Morphism](m1, m5)))
    assertThrows[IllegalArgumentException](new MorphismEquality(List[Morphism](m1, m2, m3, m4, m5)))
  }
}
