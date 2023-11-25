package cdmia.core.categorytheory.pattern.colimit

import cdmia.core.categorytheory
import cdmia.core.categorytheory.{CategoryBuilder, Object, ObjectTransformation}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.{Isomorphism, Morphism, MorphismTransformation}
import org.scalatest.funsuite.AnyFunSuite

class InitialObjectTest extends AnyFunSuite {
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
  val va2 = new Morphism("va2", vertex, a)
  val vb = new Morphism("vb", vertex, b)
  val vc = new Morphism("vc", vertex, c)
  val vd = new Morphism("vd", vertex, d)
  val av = new Morphism("av", a, vertex)
  val bv = new Morphism("bv", b, vertex)
  val cv = new Morphism("cv", c, vertex)
  val dv = new Morphism("dv", d, vertex)

  test("An initial object can be built") {
    assertResult(true)(new InitialObject(vertex).isInstanceOf[InitialObject])
  }

  test("getMorphisms returns a list with the identity morphism of the vertex") {
    assertResult(List[Morphism](vertex.identityMorphism))(new InitialObject(vertex).getMorphisms)
  }

  test("getObjects returns a list with the vertex") {
    assertResult(List[categorytheory.Object](vertex))(new InitialObject(vertex).getObjects)
  }

  test("toString produces a correct result") {
    assertResult(s"InitialObject(vertex = $vertex)")(new InitialObject(vertex).toString)
  }

  test("An initial object is valid in a category if it contains the vertex") {
    val category = CategoryBuilder("c", Iterable(vertex), Iterable()).build()
    assertResult(true)(new InitialObject(vertex).isValid(category))
  }

  test("An initial object is not valid in a category if it does not contain the vertex") {
    val category = CategoryBuilder("c", Iterable(a), Iterable()).build()
    assertResult(false)(new InitialObject(vertex).isValid(category))
  }

  test("explainIsNotValid returns an empty list if the colimit is valid") {
    val category = CategoryBuilder("c", Iterable(vertex), Iterable()).build()
    assertResult(List[String]())(new InitialObject(vertex).explainIsNotValid(category))
  }

  test("explainIsNotValid returns a list of size 1 if the colimit is not valid") {
    val category = CategoryBuilder("c", Iterable(a), Iterable()).build()
    assertResult(1)(new InitialObject(vertex).explainIsNotValid(category).size)
  }

  test("An initial object respects the universal property if it is the only object") {
    val category = CategoryBuilder("c", Iterable(vertex), Iterable()).build()
    assertResult(true)(new InitialObject(vertex).respectsUniversalProperty(category))
  }

  test("An initial object respects the universal property if there is a unique morphism to other object") {
    val category = CategoryBuilder("c", Iterable(a, vertex), Iterable(va)).build()
    assertResult(true)(new InitialObject(vertex).respectsUniversalProperty(category))
  }

  test("An initial object does not respect the universal property if there is not a unique morphism to other object") {
    val category = CategoryBuilder("c", Iterable(a, vertex), Iterable()).build()
    assertResult(false)(new InitialObject(vertex).respectsUniversalProperty(category))
  }

  test("An initial object does not respect the universal property if there the morphism to other object is not unique") {
    val category = CategoryBuilder("c", Iterable(a, vertex), Iterable(va, va2)).build()
    assertResult(false)(new InitialObject(vertex).respectsUniversalProperty(category))
  }

  test("An initial object does not respect the universal property if it has a morphism from another object") {
    val category = CategoryBuilder("c", Iterable(a, vertex), Iterable(av)).build()
    assertResult(false)(new InitialObject(vertex).respectsUniversalProperty(category))
  }

  test("An initial object respects the universal property if it is unique up to isomorphism") {
    val category = CategoryBuilder("c", Iterable(a, vertex), Iterable(va, av))
      .withIsomorphisms(Iterable(new Isomorphism(va, av))).build()
    assertResult(true)(new InitialObject(vertex).respectsUniversalProperty(category))
    assertResult(true)(new InitialObject(a).respectsUniversalProperty(category))
  }

  test("An initial object respect the universal property if it is not unique up to isomorphism") {
    val category = CategoryBuilder("c", Iterable(a, vertex), Iterable(va, av)).build()
    assertResult(false)(new InitialObject(vertex).respectsUniversalProperty(category))
    assertResult(false)(new InitialObject(a).respectsUniversalProperty(category))
  }

  test("createColimitInDestinationCategory correctly builds the colimit") {
    val category1 = CategoryBuilder("c1", Iterable(a, vertex), Iterable(va)).build()
    val category2 = CategoryBuilder("c2", Iterable(b, c), Iterable(cb)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> b, vertex ~> c),
      List[MorphismTransformation](va ~> cb)
    )
    val colimit = new InitialObject(vertex)
    val newColimit = colimit.createPatternInDestinationCategory(functor)
    assertResult(c)(newColimit.vertex)
  }

  test("createColimitsInSourceCategory correctly builds the colimit") {
    val category1 = CategoryBuilder("c1", Iterable(a, vertex), Iterable(va)).build()
    val category2 = CategoryBuilder("c2", Iterable(b, c), Iterable(cb)).build()
    val functor1 = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> b, vertex ~> c),
      List[MorphismTransformation](va ~> cb)
    )
    val colimit = new InitialObject(c)
    val newColimits1 = colimit.createPatternsInSourceCategory(functor1)
    assertResult(List(new InitialObject(vertex)))(newColimits1)

    val functor2 = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> c, vertex ~> c),
      List[MorphismTransformation](va ~> c.identityMorphism)
    )
    val newColimits2 = colimit.createPatternsInSourceCategory(functor2)
    assertResult(List(new InitialObject(a), new InitialObject(vertex)))(newColimits2)
  }

  test("isEqual returns correctly true") {
    assertResult(true)(new InitialObject(vertex) == new InitialObject(vertex))
  }

  test("isPreservedByFunctor returns true if the colimit is preserved") {
    val category1 = CategoryBuilder("c1", Iterable(a, vertex), Iterable(va)).build()
    val category2 = CategoryBuilder("c2", Iterable(b, c), Iterable(cb)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> b, vertex ~> c),
      List[MorphismTransformation](va ~> cb)
    )
    val colimit = new InitialObject(vertex)
    assertResult(true)(colimit.isPreservedByFunctor(functor))
  }

  test("isPreservedByFunctor returns false if the colimit is not preserved") {
    val category1 = CategoryBuilder("c1", Iterable(a, vertex), Iterable(va)).build()
    val category2 = CategoryBuilder("c2", Iterable(b, c), Iterable(cb)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> b, vertex ~> b),
      List[MorphismTransformation](va ~> b.identityMorphism)
    )
    val colimit = new InitialObject(vertex)
    assertResult(false)(colimit.isPreservedByFunctor(functor))
  }

  test("isPreservedByFunctor returns false if the colimit is not valid in the source category") {
    val category1 = CategoryBuilder("c1", Iterable(a, vertex), Iterable(va)).build()
    val category2 = CategoryBuilder("c2", Iterable(b, c), Iterable(cb)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> b, vertex ~> b),
      List[MorphismTransformation](va ~> b.identityMorphism)
    )
    val colimit = new InitialObject(a)
    assertResult(false)(colimit.isPreservedByFunctor(functor))
  }

  test("existsInSourceCategory returns a list of corresponding colimits if it exists in the source category") {
    val category1 = CategoryBuilder("c1", Iterable(a, vertex), Iterable(va)).build()
    val category2 = CategoryBuilder("c2", Iterable(b, c), Iterable(cb)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> b, vertex ~> c),
      List[MorphismTransformation](va ~> cb)
    )
    val colimit = new InitialObject(c)
    assertResult(List(new InitialObject(vertex)))(colimit.existsInSourceCategory(functor))
  }

  test("existsInSourceCategory returns an empty list if it does not exist in the source category") {
    val category1 = CategoryBuilder("c1", Iterable(a, b, c), Iterable(cb)).build()
    val category2 = CategoryBuilder("c2", Iterable(a, vertex), Iterable(va)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> a, c ~> vertex),
      List[MorphismTransformation](cb ~> va)
    )
    val colimit = new InitialObject(vertex)
    assertResult(List())(colimit.existsInSourceCategory(functor))
  }

  test("existsInSourceCategory returns an empty list if the colimit is not valid in the destination category") {
    val category1 = CategoryBuilder("c1", Iterable(a, vertex), Iterable(va)).build()
    val category2 = CategoryBuilder("c2", Iterable(b, c), Iterable(cb)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> b, vertex ~> b),
      List[MorphismTransformation](va ~> b.identityMorphism)
    )
    val colimit = new InitialObject(b)
    assertResult(List())(colimit.existsInSourceCategory(functor))
  }
}
