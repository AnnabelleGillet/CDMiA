package cdmia.core.categorytheory

import cdmia.core.categorytheory
import cdmia.core.categorytheory.morphism.{IdentityMorphism, Morphism}
import org.scalatest.funsuite.*

import java.lang

class ObjectTest extends AnyFunSuite {
  test("An Object should be built") {
    assertResult(true)(new Object("o").isInstanceOf[Object])
  }

  test("A identity morphism should be available with the object") {
    val a = new Object("a")

    assertResult(true)(a.identityMorphism.isInstanceOf[IdentityMorphism])
  }

  test("An object does not have the same identity morphism as another object") {
    val a = new Object("a")
    val b = new Object("b")

    assertResult(false)(a.identityMorphism == b.identityMorphism)
  }

  test("~> should produce the correct ObjectTransformation") {
    val a = new Object("a")
    val b = new Object("b")

    val ot1 = a ~> b

    assertResult(a)(ot1.source)
    assertResult(b)(ot1.destination)

    val ot2 = a ~> a

    assertResult(a)(ot2.source)
    assertResult(a)(ot2.destination)
  }

  test("isInCategory should return true if the object is in the category") {
    val a = new Object("a")
    val c = categorytheory.CategoryBuilder("c", List[Object](a), List[Morphism]()).build()

    assertResult(true)(a.isInCategory(c))
  }

  test("isInCategory should return false if the object is not in the category") {
    val a = new Object("a")
    val b = new Object("a")
    val c1 = CategoryBuilder("c", List[Object](), List[Morphism]()).build()
    val c2 = categorytheory.CategoryBuilder("c", List[Object](b), List[Morphism]()).build()

    assertResult(false)(a.isInCategory(c1))
    assertResult(false)(a.isInCategory(c2))
  }
}
