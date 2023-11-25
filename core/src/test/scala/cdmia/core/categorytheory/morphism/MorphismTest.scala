package cdmia.core.categorytheory.morphism

import cdmia.core.categorytheory
import cdmia.core.categorytheory.{CategoryBuilder, Object}
import org.scalatest.funsuite.*

class MorphismTest extends AnyFunSuite {
  val a = new Object("a")
  val b = new Object("b")
  val c = new Object("c")

  val m1 = new Morphism("m1", a, b)
  val m2 = new Morphism("m2", b, c)

  test("A Morphism should be built with domain and codomain as Object") {
    assertResult(true)(new Morphism("m", a, b).isInstanceOf[Morphism])
  }

  test("A Morphism should be built with the same Object as domain and codomain") {
    assertResult(true)(new Morphism("m", a, a).isInstanceOf[Morphism])
  }

  test("canBeComposedWith should return true is the morphisms have the same domain and codomain") {
    assertResult(true)(m1.canBeComposedWith(m2))
  }

  test("canBeComposedWith should return false is the morphisms have not the same domain and codomain") {
    assertResult(false)(m2.canBeComposedWith(m1))
  }

  test("A morphism can be composed with another morphism when codomain and domain match") {
    val d = new Object("d")
    val m = new Morphism("m", c, d)

    val mr1 = m1.composeWith(m2).composeWith(c.identityMorphism)

    assertResult("identity_c ○ m2 ○ m1")(mr1.name)
    assertResult(a)(mr1.domain)
    assertResult(c)(mr1.codomain)

    val mr2 = c.identityMorphism o m2 o m1

    assertResult("identity_c ○ m2 ○ m1")(mr2.name)
    assertResult(a)(mr2.domain)
    assertResult(c)(mr2.codomain)

    val mr3 = c.identityMorphism ○ m2 ○ m1

    assertResult("identity_c ○ m2 ○ m1")(mr3.name)
    assertResult(a)(mr3.domain)
    assertResult(c)(mr3.codomain)

    val mr4 = m1.composeWith(m2).composeWith(m)

    assertResult("m ○ m2 ○ m1")(mr4.name)
    assertResult(a)(mr4.domain)
    assertResult(d)(mr4.codomain)

    val mr5 = m o m2 o m1

    assertResult("m ○ m2 ○ m1")(mr5.name)
    assertResult(a)(mr5.domain)
    assertResult(d)(mr5.codomain)

    val mr6 = m ○ m2 ○ m1

    assertResult("m ○ m2 ○ m1")(mr6.name)
    assertResult(a)(mr6.domain)
    assertResult(d)(mr6.codomain)
  }

  test("A morphism can be composed several times") {

    val mr1 = m1.composeWith(m2)

    assertResult("m2 ○ m1")(mr1.name)
    assertResult(a)(mr1.domain)
    assertResult(c)(mr1.codomain)

    val mr2 = m2 o m1

    assertResult("m2 ○ m1")(mr2.name)
    assertResult(a)(mr2.domain)
    assertResult(c)(mr2.codomain)

    val mr3 = m2 ○ m1

    assertResult("m2 ○ m1")(mr3.name)
    assertResult(a)(mr3.domain)
    assertResult(c)(mr3.codomain)
  }

  test("~> should produce the correct MorphismTransformation") {
    val mt1 = m1 ~> m2

    assertResult(m1)(mt1.source)
    assertResult(m2)(mt1.destination)

    val mt2 = m1 ~> m1

    assertResult(m1)(mt2.source)
    assertResult(m1)(mt2.destination)
  }

  test("An IdentityMorphism should be a Morphism with the same object as domain and codomain") {
    val im = new IdentityMorphism(a)

    assertResult("identity_a")(im.name)
    assertResult(a)(im.domain)
    assertResult(a)(im.codomain)
  }

  test("isInCategory should return true if the morphism is in the category") {
    val c = CategoryBuilder("c", List[categorytheory.Object](a, b), List[Morphism](m1)).build()

    assertResult(true)(m1.isInCategory(c))
  }

  test("isInCategory should return false if the morphism is not in the category") {
    val c1 = CategoryBuilder("c", List[categorytheory.Object](), List[Morphism]()).build()
    val c2 = CategoryBuilder("c", List[categorytheory.Object](b, c), List[Morphism](m2)).build()

    assertResult(false)(m1.isInCategory(c1))
    assertResult(false)(m1.isInCategory(c2))
  }

  test("equals returns false for a comparison with an object that is not a morphism") {
    assertResult(false)(m1 == a)
  }
  
  test("toString should return the right result") {
    assertResult(s"Morphism(m1: Object(a) -> Object(b))")(m1.toString)
  }
}
