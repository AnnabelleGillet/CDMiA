package cdmia.core.categorytheory.pattern.limit

import cdmia.core.categorytheory
import cdmia.core.categorytheory.{CategoryBuilder, Object, ObjectTransformation}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.{Isomorphism, Morphism, MorphismEquality, MorphismTransformation}
import org.scalatest.funsuite.AnyFunSuite

class ProductTest extends AnyFunSuite {
  val a = new Object("a")
  val b = new Object("b")
  val c = new Object("c")
  val d = new Object("d")
  val vertex = new Object("vertex")

  val ab = new Morphism("ab", a, b)
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
  val bv = new Morphism("bv", b, vertex)
  val cv = new Morphism("cv", c, vertex)
  val dv = new Morphism("dv", d, vertex)

  test("A empty product can be built") {
    assertResult(true)(new Product(vertex, Iterable[categorytheory.Object](), Iterable[Morphism]()).isInstanceOf[Product])
  }

  test("getMorphisms returns a list with the morphisms") {
    assertResult(List[Morphism](va, vb))(new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb)).getMorphisms)
  }

  test("getObjects returns a list with the objects") {
    assertResult(List[categorytheory.Object](vertex, a, b))(new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb)).getObjects)
  }

  test("A product with one object can be built") {
    assertResult(true)(new Product(vertex, Iterable[categorytheory.Object](a), Iterable[Morphism](va)).isInstanceOf[Product])
  }

  test("A product with two object can be built") {
    assertResult(true)(new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb)).isInstanceOf[Product])
  }

  test("A product cannot be built with some objects and empty list of morphisms") {
    assertThrows[IllegalArgumentException](new Product(vertex, Iterable[categorytheory.Object](a), Iterable[Morphism]()))
  }

  test("A product cannot be built with some morphisms and empty list of objects") {
    assertThrows[IllegalArgumentException](new Product(vertex, Iterable[categorytheory.Object](), Iterable[Morphism](va)))
  }

  test("A product cannot be built with objects that are not codomain of morphisms") {
    assertThrows[IllegalArgumentException](new Product(vertex, Iterable[categorytheory.Object](b), Iterable[Morphism](va)))
  }

  test("A product cannot be built with duplicate objects") {
    assertThrows[IllegalArgumentException](new Product(vertex, Iterable[categorytheory.Object](a, a, b), Iterable[Morphism](va, vb)))
  }

  test("A product cannot be built with duplicate morphisms") {
    assertThrows[IllegalArgumentException](new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, va, vb)))
  }

  test("A product is valid in a category") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](va, vb)).build()
    val product = new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb))
    assertResult(true)(product.isValid(category))
  }

  test("A product is valid in a category with other objects and morphisms") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c, d), Iterable[Morphism](va, vb, cd, dc)).build()
    val product = new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb))
    assertResult(true)(product.isValid(category))
  }

  test("explainIsNotValid returns an empty list if the limit is valid") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](va, vb)).build()
    val product = new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb))
    assertResult(List[String]())(product.explainIsNotValid(category))
  }

  test("explainIsNotValid returns a non empty list if the limit is not valid") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](vb)).build()
    val product = new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb))
    assertResult(1)(product.explainIsNotValid(category).size)
  }

  test("A product respects the universal property when no other object can be vertex") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](va, vb)).build()
    val product = new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb))
    assertResult(true)(product.respectsUniversalProperty(category))
  }

  test("A product does not respect the universal property when another object can be vertex") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](va, vb, ca, cb)).build()
    val product = new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb))
    assertResult(false)(product.respectsUniversalProperty(category))
  }

  test("A product respects the universal property when the vertex is codomain and the diagram commutes") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](va, vb, cv, ca, cb))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ca, va o cv)), new MorphismEquality(Iterable(cb, vb o cv))))
      .build()
    val product = new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb))
    assertResult(true)(product.respectsUniversalProperty(category))
  }

  test("A product respects the universal property when the vertex is codomain and the diagram commutes, the other object having different morphisms than those of the product") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c, d), Iterable[Morphism](va, vb, cv, ca, cb, cd))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ca, va o cv)), new MorphismEquality(Iterable(cb, vb o cv))))
      .build()
    val product = new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb))
    assertResult(true)(product.respectsUniversalProperty(category))
  }

  test("A product does not respect the universal property when the vertex is codomain and the diagram does not commute") {
    val category1 = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](va, vb, cv, ca, cb))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ca, va o cv))))
      .build()
    val category2 = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](va, vb, cv, ca, cb))
      .build()
    val product = new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb))
    assertResult(false)(product.respectsUniversalProperty(category1))
    assertResult(false)(product.respectsUniversalProperty(category2))
  }

  test("A product does not respect the universal property when the vertex is domain and the diagram commute") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](va, vb, cv, ca, cb))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ca, va o cv)), new MorphismEquality(Iterable(cb, vb o cv))))
      .build()
    val product = new Product(c, Iterable[categorytheory.Object](a, b), Iterable[Morphism](ca, cb))
    assertResult(false)(product.respectsUniversalProperty(category))
  }

  test("A product respects the universal property if it is unique up to isomorphisms") {
    val category = CategoryBuilder("c", List[categorytheory.Object](vertex, a, b, c), List[Morphism](va, vb, cv, vc, ca, cb))
      .withMorphismEqualities(List[MorphismEquality](new MorphismEquality(List(ca, va o cv)), new MorphismEquality(List(cb, vb o cv)),
        new MorphismEquality(List(va, ca o vc)), new MorphismEquality(List(vb, cb o vc))))
      .withIsomorphisms(List[Isomorphism](new Isomorphism(cv, vc)))
      .build()

    val product1 = new Product(c, List[categorytheory.Object](a, b), List[Morphism](ca, cb))
    val product2 = new Product(vertex, List[categorytheory.Object](a, b), List[Morphism](va, vb))
    assertResult(true)(product1.respectsUniversalProperty(category))
    assertResult(true)(product2.respectsUniversalProperty(category))
  }

  test("A product does not respect the universal property if it is not unique up to isomorphisms") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](va, vb, cv, vc, ca, cb))
      .withMorphismEqualities(List[MorphismEquality](new MorphismEquality(List(ca, va o cv)), new MorphismEquality(List(cb, vb o cv)),
        new MorphismEquality(List(va, ca o vc)), new MorphismEquality(List(vb, cb o vc))))
      .build()
    val product1 = new Product(c, Iterable[categorytheory.Object](a, b), Iterable[Morphism](ca, cb))
    val product2 = new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb))
    assertResult(false)(product1.respectsUniversalProperty(category))
    assertResult(false)(product2.respectsUniversalProperty(category))
  }

  test("An empty product respects the universal property if it is a terminal object") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](ab, bc, cv))
      .build()
    val product = new Product(vertex, Iterable[categorytheory.Object](), Iterable[Morphism]())
    assertResult(true)(product.respectsUniversalProperty(category))
  }

  test("An empty product does not respect the universal property if it is not a terminal object") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](ab, bc, cv))
      .build()
    val product = new Product(c, Iterable[categorytheory.Object](), Iterable[Morphism]())
    assertResult(false)(product.respectsUniversalProperty(category))
  }

  test("createLimitInDestinationCategory correctly builds the limit") {
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](va, vb)).build()
    val category2 = CategoryBuilder("c2", Iterable(c, a, d), Iterable(ca, cd)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> d, vertex ~> c),
      List[MorphismTransformation](va ~> ca, vb ~> cd)
    )
    val limit = new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb))
    val newLimit = limit.createPatternInDestinationCategory(functor).asInstanceOf[Product]
    assertResult(c)(newLimit.vertex)
    assertResult(Set[categorytheory.Object](a, d))(newLimit.objects)
    assertResult(Set[Morphism](ca, cd))(newLimit.morphisms)
  }

  test("createLimitsInSourceCategory correctly builds the limit") {
    val vertex2 = new Object("v2")
    val a2 = new Object("a2")
    val b2 = new Object("b2")
    val va2 = new Morphism("va2", vertex2, a2)
    val vb2 = new Morphism("vb2", vertex2, b2)
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](va, vb)).build()
    val category2 = CategoryBuilder("c2", Iterable(c, a, d), Iterable(ca, cd)).build()
    val functor1 = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> d, vertex ~> c),
      List[MorphismTransformation](va ~> ca, vb ~> cd)
    )
    val limit = new Product(c, Iterable[categorytheory.Object](a, d), Iterable[Morphism](ca, cd))
    val newLimits1 = limit.createPatternsInSourceCategory(functor1)
    assertResult(List(new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb))))(newLimits1)

    val category3 = CategoryBuilder("c3", Iterable[categorytheory.Object](vertex, a, b, vertex2, a2, b2), Iterable[Morphism](va, vb, va2, vb2)).build()
    val functor2 = new Functor("F", category3, category2,
      List[ObjectTransformation](a ~> a, b ~> d, vertex ~> c, a2 ~> a, b2 ~> d, vertex2 ~> c),
      List[MorphismTransformation](va ~> ca, vb ~> cd, va2 ~> ca, vb2 ~> cd)
    )
    val newLimits2 = limit.createPatternsInSourceCategory(functor2)
    assertResult(List(new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb)), new Product(vertex2, Iterable[categorytheory.Object](a2, b2), Iterable[Morphism](va2, vb2))))(newLimits2)
  }

  test("isEqual returns correctly true") {
    assertResult(true)(new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb)) == new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb)))
  }

  test("isPreservedByFunctor returns true if the limit is preserved") {
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](va, vb)).build()
    val category2 = CategoryBuilder("c2", Iterable(c, a, d), Iterable(ca, cd)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> d, vertex ~> c),
      List[MorphismTransformation](va ~> ca, vb ~> cd)
    )
    val limit = new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb))
    assertResult(true)(limit.isPreservedByFunctor(functor))
  }

  test("isPreservedByFunctor returns false if the limit is not preserved") {
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](va, vb)).build()
    val category2 = CategoryBuilder("c2", Iterable(vertex, c, a, d), Iterable(ca, cd, va, vd)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> d, vertex ~> c),
      List[MorphismTransformation](va ~> ca, vb ~> cd)
    )
    val limit = new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb))
    assertResult(false)(limit.isPreservedByFunctor(functor))
  }

  test("existsInSourceCategory returns a list of corresponding limits if they exist in the source category") {
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](va, vb)).build()
    val category2 = CategoryBuilder("c2", Iterable(c, a, d), Iterable(ca, cd)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> d, vertex ~> c),
      List[MorphismTransformation](va ~> ca, vb ~> cd)
    )
    val limit = new Product(c, Iterable[categorytheory.Object](a, d), Iterable[Morphism](ca, cd))
    assertResult(List(new Product(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](va, vb))))(limit.existsInSourceCategory(functor))
  }

  test("existsInSourceCategory returns an empty list if the limit does not exist in the source category") {
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](vb)).build()
    val category2 = CategoryBuilder("c2", Iterable(vertex, c, a, d), Iterable(ca, cd, va, vd)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> d, vertex ~> c),
      List[MorphismTransformation](vb ~> cd)
    )
    val limit = new Product(c, Iterable[categorytheory.Object](a, d), Iterable[Morphism](ca, cd))
    assertResult(List())(limit.existsInSourceCategory(functor))
  }
}
