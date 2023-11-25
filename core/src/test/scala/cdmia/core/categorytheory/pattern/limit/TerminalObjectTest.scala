package cdmia.core.categorytheory.pattern.limit

import cdmia.core.categorytheory
import cdmia.core.categorytheory.{CategoryBuilder, Object, ObjectTransformation}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.{Isomorphism, Morphism, MorphismTransformation}
import org.scalatest.funsuite.AnyFunSuite

class TerminalObjectTest extends AnyFunSuite {
  val a = new Object("a")
  val b = new Object("b")
  val c = new Object("c")
  val d = new Object("d")
  val vertex = new Object("vertex")

  val ab = new Morphism("ab", a, b)
  val ab2 = new Morphism("ab2", a, b)
  val ba = new Morphism("ba", b, a)
  val bc = new Morphism("bc", b, c)
  val ca = new Morphism("ca", c, a)
  val cb = new Morphism("cb", c, b)
  val cd = new Morphism("cd", c, d)
  val dc = new Morphism("dc", d, c)
  val va = new Morphism("va", vertex, a)
  val vb = new Morphism("vb", vertex, b)
  val vc = new Morphism("vc", vertex, c)
  val vd = new Morphism("vd", vertex, d)
  val av = new Morphism("av", a, vertex)
  val av2 = new Morphism("av2", a, vertex)
  val bv = new Morphism("bv", b, vertex)
  val cv = new Morphism("cv", c, vertex)
  val dv = new Morphism("dv", d, vertex)

  test("A terminal object can be built") {
    assertResult(true)(new TerminalObject(vertex).isInstanceOf[TerminalObject])
  }

  test("getMorphisms returns a list with the identity morphism of the vertex") {
    assertResult(List[Morphism](vertex.identityMorphism))(new TerminalObject(vertex).getMorphisms)
  }

  test("getObjects returns a list with the vertex") {
    assertResult(List[categorytheory.Object](vertex))(new TerminalObject(vertex).getObjects)
  }

  test("toString produces a correct result") {
    assertResult(s"TerminalObject(vertex = $vertex)")(new TerminalObject(vertex).toString)
  }

  test("A terminal object is valid in a category if it contains the vertex") {
    val category = CategoryBuilder("c", Iterable(vertex), Iterable()).build()
    assertResult(true)(new TerminalObject(vertex).isValid(category))
  }

  test("A terminal object is not valid in a category if it does not contain the vertex") {
    val category = CategoryBuilder("c", Iterable(a), Iterable()).build()
    assertResult(false)(new TerminalObject(vertex).isValid(category))
  }

  test("explainIsNotValid returns an empty list if the limit is valid") {
    val category = CategoryBuilder("c", Iterable(vertex), Iterable()).build()
    assertResult(List[String]())(new TerminalObject(vertex).explainIsNotValid(category))
  }

  test("explainIsNotValid returns a list of size 1 if the limit is not valid") {
    val category = CategoryBuilder("c", Iterable(a), Iterable()).build()
    assertResult(1)(new TerminalObject(vertex).explainIsNotValid(category).size)
  }

  test("A terminal object respects the universal property if it is the only object") {
    val category = CategoryBuilder("c", Iterable(vertex), Iterable()).build()
    assertResult(true)(new TerminalObject(vertex).respectsUniversalProperty(category))
  }

  test("A terminal object respects the universal property if there is a unique morphism from other object") {
    val category = CategoryBuilder("c", Iterable(a, vertex), Iterable(av)).build()
    assertResult(true)(new TerminalObject(vertex).respectsUniversalProperty(category))
  }

  test("A terminal object does not respect the universal property if there is not a unique morphism from other object") {
    val category = CategoryBuilder("c", Iterable(a, vertex), Iterable()).build()
    assertResult(false)(new TerminalObject(vertex).respectsUniversalProperty(category))
  }

  test("A terminal object does not respect the universal property if there the morphism from other object is not unique") {
    val category = CategoryBuilder("c", Iterable(a, vertex), Iterable(av, av2)).build()
    assertResult(false)(new TerminalObject(vertex).respectsUniversalProperty(category))
  }

  test("A terminal object does not respect the universal property if it has a morphism towards another object") {
    val category = CategoryBuilder("c", Iterable(a, vertex), Iterable(va)).build()
    assertResult(false)(new TerminalObject(vertex).respectsUniversalProperty(category))
  }

  test("A terminal object respects the universal property if it is unique up to isomorphism") {
    val category = CategoryBuilder("c", Iterable(a, vertex), Iterable(va, av))
      .withIsomorphisms(Iterable(new Isomorphism(va, av))).build()
    assertResult(true)(new TerminalObject(vertex).respectsUniversalProperty(category))
    assertResult(true)(new TerminalObject(a).respectsUniversalProperty(category))
  }

  test("A terminal object respect the universal property if it is not unique up to isomorphism") {
    val category = CategoryBuilder("c", Iterable(a, vertex), Iterable(va, av)).build()
    assertResult(false)(new TerminalObject(vertex).respectsUniversalProperty(category))
    assertResult(false)(new TerminalObject(a).respectsUniversalProperty(category))
  }

  test("createLimitInDestinationCategory correctly builds the limit") {
    val category1 = CategoryBuilder("c1", Iterable(a, vertex), Iterable(av)).build()
    val category2 = CategoryBuilder("c2", Iterable(b, c), Iterable(bc)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> b, vertex ~> c),
      List[MorphismTransformation](av ~> bc)
    )
    val limit = new TerminalObject(vertex)
    val newLimit = limit.createPatternInDestinationCategory(functor)
    assertResult(c)(newLimit.vertex)
  }

  test("createLimitsInSourceCategory correctly builds the limit") {
    val category1 = CategoryBuilder("c1", Iterable(a, vertex), Iterable(av)).build()
    val category2 = CategoryBuilder("c2", Iterable(b, c), Iterable(bc)).build()
    val functor1 = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> b, vertex ~> c),
      List[MorphismTransformation](av ~> bc)
    )
    val limit = new TerminalObject(c)
    val newLimit1 = limit.createPatternsInSourceCategory(functor1)
    assertResult(List(new TerminalObject(vertex)))(newLimit1)

    val functor2 = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> c, vertex ~> c),
      List[MorphismTransformation](av ~> c.identityMorphism)
    )
    val newLimit2 = limit.createPatternsInSourceCategory(functor2)
    assertResult(List(new TerminalObject(a), new TerminalObject(vertex)))(newLimit2)
  }

  test("isEqual returns correctly true") {
    assertResult(true)(new TerminalObject(vertex) == new TerminalObject(vertex))
  }

  test("isPreservedByFunctor returns true if the limit is preserved") {
    val category1 = CategoryBuilder("c1", Iterable(a, vertex), Iterable(av)).build()
    val category2 = CategoryBuilder("c2", Iterable(b, c), Iterable(bc)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> b, vertex ~> c),
      List[MorphismTransformation](av ~> bc)
    )
    val limit = new TerminalObject(vertex)
    assertResult(true)(limit.isPreservedByFunctor(functor))
  }

  test("isPreservedByFunctor returns false if the limit is not preserved") {
    val category1 = CategoryBuilder("c1", Iterable(a, vertex), Iterable(av)).build()
    val category2 = CategoryBuilder("c2", Iterable(b, c), Iterable(bc)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> b, vertex ~> b),
      List[MorphismTransformation](av ~> b.identityMorphism)
    )
    val limit = new TerminalObject(vertex)
    assertResult(false)(limit.isPreservedByFunctor(functor))
  }

  test("isPreservedByFunctor returns false if the limit is not valid in the source category") {
    val category1 = CategoryBuilder("c1", Iterable(a, vertex), Iterable(av)).build()
    val category2 = CategoryBuilder("c2", Iterable(b, c), Iterable(bc)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> b, vertex ~> b),
      List[MorphismTransformation](av ~> b.identityMorphism)
    )
    val limit = new TerminalObject(a)
    assertResult(false)(limit.isPreservedByFunctor(functor))
  }

  test("existsInSourceCategory returns a list of corresponding limits if it exists in the source category") {
    val category1 = CategoryBuilder("c1", Iterable(a, vertex), Iterable(av)).build()
    val category2 = CategoryBuilder("c2", Iterable(b, c), Iterable(bc)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> b, vertex ~> c),
      List[MorphismTransformation](av ~> bc)
    )
    val limit = new TerminalObject(c)
    assertResult(List(new TerminalObject(vertex)))(limit.existsInSourceCategory(functor))
  }

  test("existsInSourceCategory returns an empty list if it does not exist in the source category") {
    val category1 = CategoryBuilder("c1", Iterable(a, b, c), Iterable(bc)).build()
    val category2 = CategoryBuilder("c2", Iterable(a, vertex), Iterable(av)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> a, c ~> vertex),
      List[MorphismTransformation](bc ~> av)
    )
    val limit = new TerminalObject(vertex)
    assertResult(List())(limit.existsInSourceCategory(functor))
  }

  test("existsInSourceCategory returns an empty list if the limit is not valid in the destination category") {
    val category1 = CategoryBuilder("c1", Iterable(a, vertex), Iterable(av)).build()
    val category2 = CategoryBuilder("c2", Iterable(b, c), Iterable(bc)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> b, vertex ~> b),
      List[MorphismTransformation](av ~> b.identityMorphism)
    )
    val limit = new TerminalObject(b)
    assertResult(List())(limit.existsInSourceCategory(functor))
  }
}
