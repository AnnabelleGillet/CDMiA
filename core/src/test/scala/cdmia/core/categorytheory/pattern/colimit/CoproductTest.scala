package cdmia.core.categorytheory.pattern.colimit

import cdmia.core.categorytheory
import cdmia.core.categorytheory.{CategoryBuilder, Object, ObjectTransformation}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.{Isomorphism, Morphism, MorphismEquality, MorphismTransformation}
import org.scalatest.funsuite.AnyFunSuite

class CoproductTest extends AnyFunSuite {
  val a = new Object("a")
  val b = new Object("b")
  val c = new Object("c")
  val d = new Object("d")
  val vertex = new Object("vertex")

  val ab = new Morphism("ab", a, b)
  val ac = new Morphism("ac", a, c)
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

  test("A empty coproduct can be built") {
    assertResult(true)(new Coproduct(vertex, Iterable[categorytheory.Object](), Iterable[Morphism]()).isInstanceOf[Coproduct])
  }

  test("getMorphisms returns a list with the morphisms") {
    assertResult(List[Morphism](av, bv))(new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv)).getMorphisms)
  }

  test("getObjects returns a list with the objects") {
    assertResult(List[categorytheory.Object](vertex, a, b))(new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv)).getObjects)
  }

  test("A coproduct with one object can be built") {
    assertResult(true)(new Coproduct(vertex, Iterable[categorytheory.Object](a), Iterable[Morphism](av)).isInstanceOf[Coproduct])
  }

  test("A coproduct with two object can be built") {
    assertResult(true)(new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv)).isInstanceOf[Coproduct])
  }

  test("A coproduct cannot be built with some objects and empty list of morphisms") {
    assertThrows[IllegalArgumentException](new Coproduct(vertex, Iterable[categorytheory.Object](a), Iterable[Morphism]()))
  }

  test("A coproduct cannot be built with some morphisms and empty list of objects") {
    assertThrows[IllegalArgumentException](new Coproduct(vertex, Iterable[categorytheory.Object](), Iterable[Morphism](av)))
  }

  test("A coproduct cannot be built with objects that are not domain of morphisms") {
    assertThrows[IllegalArgumentException](new Coproduct(vertex, Iterable[categorytheory.Object](b), Iterable[Morphism](av)))
  }

  test("A coproduct cannot be built with duplicate objects") {
    assertThrows[IllegalArgumentException](new Coproduct(vertex, Iterable[categorytheory.Object](a, a, b), Iterable[Morphism](av, bv)))
  }

  test("A coproduct cannot be built with duplicate morphisms") {
    assertThrows[IllegalArgumentException](new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, av, bv)))
  }

  test("A coproduct is valid in a category") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](av, bv)).build()
    val coproduct = new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv))
    assertResult(true) (coproduct.isValid(category))
  }

  test("A coproduct is valid in a category with other objects and morphisms") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c, d), Iterable[Morphism](av, bv, cd, dc)).build()
    val coproduct = new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv))
    assertResult(true) (coproduct.isValid(category))
  }

  test("explainIsNotValid returns an empty list if the limit is valid") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](av, bv)).build()
    val coproduct = new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv))
    assertResult(List[String]())(coproduct.explainIsNotValid(category))
  }

  test("explainIsNotValid returns a non empty list if the limit is not valid") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](av)).build()
    val coproduct = new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv))
    assertResult(1)(coproduct.explainIsNotValid(category).size)
  }

  test("A coproduct respects the universal property when no other object can be vertex") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](av, bv)).build()
    val coproduct = new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv))
    assertResult(true) (coproduct.respectsUniversalProperty(category))
  }

  test("A coproduct does not respect the universal property when another object can be vertex") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](av, bv, ac, bc)).build()
    val coproduct = new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv))
    assertResult(false)(coproduct.respectsUniversalProperty(category))
  }

  test("A coproduct respects the universal property when the vertex is domain and the diagram commutes") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](av, bv, vc, ac, bc))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac, vc o av)), new MorphismEquality(Iterable(bc, vc o bv))))
      .build()
    val coproduct = new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv))
    assertResult(true) (coproduct.respectsUniversalProperty(category))
  }

  test("A coproduct respects the universal property when the vertex is codomain and the diagram commutes, the other object having different morphisms than those of the coproduct") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c, d), Iterable[Morphism](av, bv, vc, ac, bc, dc))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac, vc o av)), new MorphismEquality(Iterable(bc, vc o bv))))
      .build()
    val coproduct = new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv))
    assertResult(true) (coproduct.respectsUniversalProperty(category))
  }

  test("A coproduct does not respect the universal property when the vertex is codomain and the diagram does not commute") {
    val category1 = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](av, bv, vc, ac, bc))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac, vc o av))))
      .build()
    val category2 = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](av, bv, vc, ac, bc))
      .build()
    val coproduct = new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv))
    assertResult(false) (coproduct.respectsUniversalProperty(category1))
    assertResult(false) (coproduct.respectsUniversalProperty(category2))
  }

  test("A coproduct does not respect the universal property when the vertex is domain and the diagram commute") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](av, bv, vc, ac, bc))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac, vc o av)), new MorphismEquality(Iterable(bc, vc o bv))))
      .build()
    val coproduct = new Coproduct(c, Iterable[categorytheory.Object](a, b), Iterable[Morphism](ac, bc))
    assertResult(false)(coproduct.respectsUniversalProperty(category))
  }

  test("A coproduct respects the universal property if it is unique up to isomorphisms") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](av, bv, cv, vc, ac, bc))
      .withIsomorphisms(Iterable(new Isomorphism(cv, vc)))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac, vc o av)), new MorphismEquality(Iterable(bc, vc o bv)),
        new MorphismEquality(Iterable(av, cv o ac)), new MorphismEquality(Iterable(bv, cv o bc))))
      .build()
    val coproduct1 = new Coproduct(c, Iterable[categorytheory.Object](a, b), Iterable[Morphism](ac, bc))
    val coproduct2 = new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv))
    assertResult(true)(coproduct1.respectsUniversalProperty(category))
    assertResult(true)(coproduct2.respectsUniversalProperty(category))
  }

  test("A coproduct does not respect the universal property if it is not unique up to isomorphisms") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](av, bv, cv, vc, ac, bc))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac, vc o av)), new MorphismEquality(Iterable(bc, vc o bv)),
        new MorphismEquality(Iterable(av, cv o ac)), new MorphismEquality(Iterable(bv, cv o bc))))
      .build()
    val coproduct1 = new Coproduct(c, Iterable[categorytheory.Object](a, b), Iterable[Morphism](ac, bc))
    val coproduct2 = new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv))
    assertResult(false)(coproduct1.respectsUniversalProperty(category))
    assertResult(false)(coproduct2.respectsUniversalProperty(category))
  }

  test("An empty coproduct respects the universal property if it is an initial object") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](ba, cb, vc))
      .build()
    val coproduct = new Coproduct(vertex, Iterable[categorytheory.Object](), Iterable[Morphism]())
    assertResult(true)(coproduct.respectsUniversalProperty(category))
  }

  test("An empty coproduct does not respect the universal property if it is not an initial object") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](ba, cb, vc))
      .build()
    val coproduct = new Coproduct(c, Iterable[categorytheory.Object](), Iterable[Morphism]())
    assertResult(false)(coproduct.respectsUniversalProperty(category))
  }

  test("createColimitInDestinationCategory correctly builds the colimit") {
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](av, bv)).build()
    val category2 = CategoryBuilder("c2", Iterable(c, a, d), Iterable(ac, dc)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> d, vertex ~> c),
      List[MorphismTransformation](av ~> ac, bv ~> dc)
    )
    val colimit = new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv))
    val newColimit = colimit.createPatternInDestinationCategory(functor).asInstanceOf[Coproduct]
    assertResult(c)(newColimit.vertex)
    assertResult(Set[categorytheory.Object](a, d))(newColimit.objects)
    assertResult(Set[Morphism](ac, dc))(newColimit.morphisms)
  }

  test("createColimitsInSourceCategory correctly builds the limit") {
    val vertex2 = new Object("v2")
    val a2 = new Object("a2")
    val b2 = new Object("b2")
    val av2 = new Morphism("av2", a2, vertex2)
    val bv2 = new Morphism("bv2", b2, vertex2)
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](av, bv)).build()
    val category2 = CategoryBuilder("c2", Iterable(c, a, d), Iterable(ac, dc)).build()
    val functor1 = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> d, vertex ~> c),
      List[MorphismTransformation](av ~> ac, bv ~> dc)
    )
    val colimit = new Coproduct(c, Iterable[categorytheory.Object](a, d), Iterable[Morphism](ac, dc))
    val newColimits1 = colimit.createPatternsInSourceCategory(functor1)
    assertResult(List(new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv))))(newColimits1)

    val category3 = CategoryBuilder("c3", Iterable[categorytheory.Object](vertex, a, b, vertex2, a2, b2), Iterable[Morphism](av, bv, av2, bv2)).build()
    val functor2 = new Functor("F", category3, category2,
      List[ObjectTransformation](a ~> a, b ~> d, vertex ~> c, a2 ~> a, b2 ~> d, vertex2 ~> c),
      List[MorphismTransformation](av ~> ac, bv ~> dc, av2 ~> ac, bv2 ~> dc)
    )
    val newColimits2 = colimit.createPatternsInSourceCategory(functor2)
    assertResult(List(new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv)), new Coproduct(vertex2, Iterable[categorytheory.Object](a2, b2), Iterable[Morphism](av2, bv2))))(newColimits2)
  }

  test("isEqual returns correctly true") {
    assertResult(true)(new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv)) == new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv)))
  }

  test("isPreservedByFunctor returns true if the colimit is preserved") {
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](av, bv)).build()
    val category2 = CategoryBuilder("c2", Iterable(c, a, d), Iterable(ac, dc)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> d, vertex ~> c),
      List[MorphismTransformation](av ~> ac, bv ~> dc)
    )
    val colimit = new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv))
    assertResult(true)(colimit.isPreservedByFunctor(functor))
  }

  test("isPreservedByFunctor returns false if the colimit is not preserved") {
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](av, bv)).build()
    val category2 = CategoryBuilder("c2", Iterable(vertex, c, a, d), Iterable(ac, dc, av, dv)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> d, vertex ~> c),
      List[MorphismTransformation](av ~> ac, bv ~> dc)
    )
    val colimit = new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv))
    assertResult(false)(colimit.isPreservedByFunctor(functor))
  }

  test("existsInSourceCategory returns a list of corresponding colimits if they exist in the source category") {
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](av, bv)).build()
    val category2 = CategoryBuilder("c2", Iterable(c, a, d), Iterable(ac, dc)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> d, vertex ~> c),
      List[MorphismTransformation](av ~> ac, bv ~> dc)
    )
    val colimit = new Coproduct(c, Iterable[categorytheory.Object](a, d), Iterable[Morphism](ac, dc))
    assertResult(List(new Coproduct(vertex, Iterable[categorytheory.Object](a, b), Iterable[Morphism](av, bv))))(colimit.existsInSourceCategory(functor))
  }

  test("existsInSourceCategory returns an empty list if the colimit does not exist in the source category") {
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](bv)).build()
    val category2 = CategoryBuilder("c2", Iterable(vertex, c, a, d), Iterable(ac, dc, av, dv)).build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> d, vertex ~> c),
      List[MorphismTransformation](bv ~> dc)
    )
    val colimit = new Coproduct(c, Iterable[categorytheory.Object](a, d), Iterable[Morphism](ac, dc))
    assertResult(List())(colimit.existsInSourceCategory(functor))
  }
}