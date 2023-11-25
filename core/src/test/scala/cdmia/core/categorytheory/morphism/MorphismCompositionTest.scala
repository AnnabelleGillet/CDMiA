package cdmia.core.categorytheory.morphism

import cdmia.core.categorytheory
import cdmia.core.categorytheory.{CategoryBuilder, Object}
import org.scalatest.funsuite.*

class MorphismCompositionTest extends AnyFunSuite {
  val a = new Object("a")
  val b = new Object("b")
  val m1 = new Morphism("m1", a, b)
  val m2 = new Morphism("m2", b, a)
  val m3 = new Morphism("m3", a, a)

  test("A MorphismComposition should be correctly built") {
    assertResult(true)(new MorphismComposition(m1, m2).isInstanceOf[MorphismComposition])
    assertResult(true)(new MorphismComposition(m3, m1).isInstanceOf[MorphismComposition])
    assertResult(true)(new MorphismComposition(m2, m3).isInstanceOf[MorphismComposition])
    assertResult(true)(new MorphismComposition(m3, m3).isInstanceOf[MorphismComposition])
  }

  test("A MorphismComposition should be correctly built with multiple compositions") {
    assertResult(true)(new MorphismComposition(new MorphismComposition(m3, m1), m2).isInstanceOf[MorphismComposition])
    assertResult(true)(new MorphismComposition(m3, new MorphismComposition(m1, m2)).isInstanceOf[MorphismComposition])
    assertResult(true)(new MorphismComposition(new MorphismComposition(m1, m2), new MorphismComposition(m3, m1)).isInstanceOf[MorphismComposition])

    assertResult(true)(new MorphismComposition(new MorphismComposition(m3, m3), m3).isInstanceOf[MorphismComposition])
    assertResult(true)(new MorphismComposition(m3, new MorphismComposition(m3, m3)).isInstanceOf[MorphismComposition])
    assertResult(true)(new MorphismComposition(new MorphismComposition(m3, m3), new MorphismComposition(m3, m3)).isInstanceOf[MorphismComposition])
  }

  test("A MorphismComposition should not be built when codomain and domain do not match") {
    assertThrows[IllegalArgumentException](new MorphismComposition(m1, m3))
    assertThrows[IllegalArgumentException](new MorphismComposition(m3, m2))
  }

  test("A MorphismComposition should produce the correct chain of morphisms") {
    assertResult(List[Morphism](m1, m2))(new MorphismComposition(m1, m2).chainOfMorphisms)
    assertResult(List[Morphism](m3, m1))(new MorphismComposition(m3, m1).chainOfMorphisms)
    assertResult(List[Morphism](m2, m3))(new MorphismComposition(m2, m3).chainOfMorphisms)
    assertResult(List[Morphism](m3, m3))(new MorphismComposition(m3, m3).chainOfMorphisms)
  }

  test("A MorphismComposition should produce the correct chain of morphisms with multiple compositions") {
    assertResult(List[Morphism](m3, m1, m2))(new MorphismComposition(new MorphismComposition(m3, m1), m2).chainOfMorphisms)
    assertResult(List[Morphism](m3, m1, m2))(new MorphismComposition(m3, new MorphismComposition(m1, m2)).chainOfMorphisms)
    assertResult(List[Morphism](m1, m2, m3, m1))(new MorphismComposition(new MorphismComposition(m1, m2), new MorphismComposition(m3, m1)).chainOfMorphisms)

    assertResult(List[Morphism](m3, m3, m3))(new MorphismComposition(new MorphismComposition(m3, m3), m3).chainOfMorphisms)
    assertResult(List[Morphism](m3, m3, m3))(new MorphismComposition(m3, new MorphismComposition(m3, m3)).chainOfMorphisms)
    assertResult(List[Morphism](m3, m3, m3, m3))(new MorphismComposition(new MorphismComposition(m3, m3), new MorphismComposition(m3, m3)).chainOfMorphisms)
  }

  test("isInCategory should return true if the composition is in the category") {
    val c = CategoryBuilder("c", List[categorytheory.Object](a, b), List[Morphism](m1, m2)).build()
    val composition = new MorphismComposition(m1, m2)

    assertResult(true)(composition.isInCategory(c))
  }

  test("isInCategory should return false if the composition is not in the category") {
    val c1 = CategoryBuilder("c", List[categorytheory.Object](), List[Morphism]()).build()
    val c2 = CategoryBuilder("c", List[categorytheory.Object](a, b), List[Morphism](m2, m3)).build()
    val composition = new MorphismComposition(m1, m2)

    assertResult(false)(composition.isInCategory(c1))
    assertResult(false)(composition.isInCategory(c2))
  }

  test("equals should return true if two MorphismComposition have the same morphisms") {
    assertResult(true)((m1 o m2) == (m1 o m2))
  }

  test("equals should return true if two MorphismComposition have the same morphisms and identity morphisms") {
    assertResult(true)((m1 o a.identityMorphism o m2) == (m1 o m2))
    assertResult(true)((m1 o m2) == (m1 o a.identityMorphism o m2))
    assertResult(true)((m1 o a.identityMorphism o m2) == (m1 o a.identityMorphism o m2))
  }

  test("equals should return false if two MorphismComposition do not have the same morphisms") {
    assertResult(false)((m1 o m2) == (m1 o m3))
  }

  test("equals should return true if two it is f o id_x or id_y o f and is compared with f") {
    assertResult(true)((m1 o a.identityMorphism) == m1)
    assertResult(true)((m1 o a.identityMorphism o a.identityMorphism) == m1)
    assertResult(true)((b.identityMorphism o m1) == m1)

    assertResult(true)(m1 == (m1 o a.identityMorphism))
    assertResult(true)(m1 == (m1 o a.identityMorphism o a.identityMorphism))
    assertResult(true)(m1 == (b.identityMorphism o m1))

    assertResult(true)(m1 == m1)
  }

  test("equals should return false if it is not f o id_x or id_y o f and is compared with a morphism") {
    assertResult(false)((m1 o m2) == m1)
  }

  test("equals should return false if it is compared with something else than a morphism") {
    assertResult(false)((m1 o m2) == a)
  }
}
