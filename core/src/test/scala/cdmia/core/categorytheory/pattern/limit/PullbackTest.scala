package cdmia.core.categorytheory.pattern.limit

import cdmia.core.categorytheory
import cdmia.core.categorytheory.{CategoryBuilder, Object, ObjectTransformation}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.{Isomorphism, Morphism, MorphismEquality, MorphismTransformation}
import org.scalatest.funsuite.AnyFunSuite

class PullbackTest extends AnyFunSuite {
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
  val da = new Morphism("da", d, a)
  val db = new Morphism("db", d, b)
  val dc = new Morphism("dc", d, c)
  val va = new Morphism("va", vertex, a)
  val vb = new Morphism("vb", vertex, b)
  val vc = new Morphism("vc", vertex, c)
  val vd = new Morphism("vd", vertex, d)
  val av = new Morphism("av", a, vertex)
  val bv = new Morphism("bv", b, vertex)
  val cv = new Morphism("cv", c, vertex)
  val dv = new Morphism("dv", d, vertex)

  test("A pullback can be built") {
    assertResult(true)(new Pullback(vertex, a, b, c, va, vb, ac, bc).isInstanceOf[Pullback])
  }

  test("getMorphisms returns a list with the morphisms") {
    assertResult(List[Morphism](va, vb, ac, bc))(new Pullback(vertex, a, b, c, va, vb, ac, bc).getMorphisms)
  }

  test("getObjects returns a list with the objects") {
    assertResult(List[categorytheory.Object](vertex, a, b, c))(new Pullback(vertex, a, b, c, va, vb, ac, bc).getObjects)
  }

  test("A pullback cannot be built when va does not match vertex and a") {
    assertThrows[IllegalArgumentException](new Pullback(vertex, a, b, c, av, vb, ac, bc))
  }

  test("A pullback cannot be built when vb does not match vertex and b") {
    assertThrows[IllegalArgumentException](new Pullback(vertex, a, b, c, va, bv, ac, bc))
  }

  test("A pullback cannot be built when ac does not match a and c") {
    assertThrows[IllegalArgumentException](new Pullback(vertex, a, b, c, av, vb, ab, bc))
  }

  test("A pullback cannot be built when bc does not match b and c") {
    assertThrows[IllegalArgumentException](new Pullback(vertex, a, b, c, av, vb, ac, cd))
  }

  test("A pullback is valid in a category") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](va, vb, ac, bc))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb))))
      .build()
    val pullback = new Pullback(vertex, a, b, c, va, vb, ac, bc)
    assertResult(true)(pullback.isValid(category))
  }

  test("A pullback is valid in a category with other objects and morphisms") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c, d), Iterable[Morphism](va, vb, ac, bc, cd))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb))))
      .build()
    val pullback = new Pullback(vertex, a, b, c, va, vb, ac, bc)
    assertResult(true)(pullback.isValid(category))
  }

  test("A pullback is not valid in a category when some objects are missing") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b), Iterable[Morphism](va, vb))
      .build()
    val pullback = new Pullback(vertex, a, b, c, va, vb, ac, bc)
    assertResult(false)(pullback.isValid(category))
  }

  test("A pullback is not valid in a category when the diagram does not commute") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c, d), Iterable[Morphism](va, vb, ac, bc, cd))
      .build()
    val pullback = new Pullback(vertex, a, b, c, va, vb, ac, bc)
    assertResult(false)(pullback.isValid(category))
  }

  test("explainIsNotValid returns an empty list if the limit is valid") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](va, vb, ac, bc))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb))))
      .build()
    val pullback = new Pullback(vertex, a, b, c, va, vb, ac, bc)
    assertResult(List[String]())(pullback.explainIsNotValid(category))
  }

  test("explainIsNotValid returns a non-empty list if the limit is not valid") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](va, vb, ac, bc))
      .build()
    val pullback = new Pullback(vertex, a, b, c, va, vb, ac, bc)
    assertResult(1)(pullback.explainIsNotValid(category).size)
  }

  test("A pullback respects the universal property when no other object can be vertex") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](va, vb, ac, bc))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb))))
      .build()
    val pullback = new Pullback(vertex, a, b, c, va, vb, ac, bc)
    assertResult(true)(pullback.respectsUniversalProperty(category))
  }

  test("A pullback does not respect the universal property when another object can be vertex") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c, d), Iterable[Morphism](va, vb, ac, bc, da, db))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb)), new MorphismEquality(Iterable(ac o da, bc o db))))
      .build()
    val pullback = new Pullback(vertex, a, b, c, va, vb, ac, bc)
    assertResult(false)(pullback.respectsUniversalProperty(category))
  }

  test("A pullback respects the universal property when the vertex is codomain and the diagram commutes") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c, d), Iterable[Morphism](va, vb, ac, bc, da, db, dv))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb)), new MorphismEquality(Iterable(ac o da, bc o db)),
        new MorphismEquality(Iterable(da, va o dv)), new MorphismEquality(Iterable(db, vb o dv))))
      .build()
    val pullback = new Pullback(vertex, a, b, c, va, vb, ac, bc)
    assertResult(true)(pullback.respectsUniversalProperty(category))
  }

  test("A pullback respects the universal property when the vertex is codomain and the diagram commutes, the other object having different morphisms than those of the pullback") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c, d), Iterable[Morphism](va, vb, ac, bc, dc, dv))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb))))
      .build()
    val pullback = new Pullback(vertex, a, b, c, va, vb, ac, bc)
    assertResult(true)(pullback.respectsUniversalProperty(category))
  }

  test("A pullback does not respect the universal property when the vertex is codomain and the diagram does not commute, the other object having only one morphism towards an object of the pullback") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c, d), Iterable[Morphism](va, vb, ac, bc, da, dc, dv))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb))))
      .build()
    val pullback = new Pullback(vertex, a, b, c, va, vb, ac, bc)
    assertResult(false)(pullback.respectsUniversalProperty(category))
  }

  test("A pullback does not respect the universal property when the vertex is codomain and the diagram does not commute") {
    val category1 = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c, d), Iterable[Morphism](va, vb, ac, bc, da, db, dv))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb)), new MorphismEquality(Iterable(ac o da, bc o db)),
        new MorphismEquality(Iterable(da, va o dv))))
      .build()
    val category2 = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c, d), Iterable[Morphism](va, vb, ac, bc, da, db, dv))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb)), new MorphismEquality(Iterable(ac o da, bc o db)),
        new MorphismEquality(Iterable(db, vb o dv))))
      .build()
    val category3 = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c, d), Iterable[Morphism](va, vb, ac, bc, da, db, dv))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb)), new MorphismEquality(Iterable(ac o da, bc o db))))
      .build()
    val pullback = new Pullback(vertex, a, b, c, va, vb, ac, bc)
    assertResult(false)(pullback.respectsUniversalProperty(category1))
    assertResult(false)(pullback.respectsUniversalProperty(category2))
    assertResult(false)(pullback.respectsUniversalProperty(category3))
  }

  test("A pullback does not respect the universal property when the vertex is domain and the diagram commute") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c, d), Iterable[Morphism](va, vb, ac, bc, da, db, dv))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb)), new MorphismEquality(Iterable(ac o da, bc o db)),
        new MorphismEquality(Iterable(da, va o dv)), new MorphismEquality(Iterable(db, vb o dv))))
      .build()
    val pullback = new Pullback(d, a, b, c, da, db, ac, bc)
    assertResult(false)(pullback.respectsUniversalProperty(category))
  }

  test("A pullback respects the universal property if it is unique up to isomorphisms") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c, d), Iterable[Morphism](va, vb, ac, bc, da, db, dv, vd))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb)), new MorphismEquality(Iterable(ac o da, bc o db)),
        new MorphismEquality(Iterable(da, va o dv)), new MorphismEquality(Iterable(db, vb o dv)),
        new MorphismEquality(Iterable(va, da o vd)), new MorphismEquality(Iterable(vb, db o vd))))
      .withIsomorphisms(Iterable[Isomorphism](new Isomorphism(dv, vd)))
      .build()
    val pullback1 = new Pullback(vertex, a, b, c, va, vb, ac, bc)
    val pullback2 = new Pullback(d, a, b, c, da, db, ac, bc)
    assertResult(true)(pullback1.respectsUniversalProperty(category))
    assertResult(true)(pullback2.respectsUniversalProperty(category))
  }

  test("A pullback does not respect the universal property if it is not unique up to isomorphisms") {
    val category = CategoryBuilder("c", Iterable[categorytheory.Object](vertex, a, b, c, d), Iterable[Morphism](va, vb, ac, bc, da, db, dv, vd))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb)), new MorphismEquality(Iterable(ac o da, bc o db)),
        new MorphismEquality(Iterable(da, va o dv)), new MorphismEquality(Iterable(db, vb o dv)),
        new MorphismEquality(Iterable(va, da o vd)), new MorphismEquality(Iterable(vb, db o vd))))
      .build()
    val pullback1 = new Pullback(vertex, a, b, c, va, vb, ac, bc)
    val pullback2 = new Pullback(d, a, b, c, da, db, ac, bc)
    assertResult(false)(pullback1.respectsUniversalProperty(category))
    assertResult(false)(pullback2.respectsUniversalProperty(category))
  }

  test("createLimitInDestinationCategory correctly builds the limit") {
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](va, vb, ac, bc))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb))))
      .build()
    val category2 = CategoryBuilder("c2", Iterable[categorytheory.Object](d, a, b, vertex), Iterable[Morphism](da, db, av, bv))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o da, bv o db))))
      .build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> b, c ~> vertex, vertex ~> d),
      List[MorphismTransformation](va ~> da, vb ~> db, ac ~> av, bc ~> bv)
    )
    val limit = new Pullback(vertex, a, b, c, va, vb, ac, bc)
    val newLimit = limit.createPatternInDestinationCategory(functor).asInstanceOf[Pullback]
    assertResult(d)(newLimit.vertex)
    assertResult(a)(newLimit.a)
    assertResult(b)(newLimit.b)
    assertResult(vertex)(newLimit.c)
    assertResult(da)(newLimit.va)
    assertResult(db)(newLimit.vb)
    assertResult(av)(newLimit.ac)
    assertResult(bv)(newLimit.bc)
  }

  test("createLimitsInSourceCategory correctly builds the limit") {
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](va, vb, ac, bc))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb))))
      .build()
    val category2 = CategoryBuilder("c2", Iterable[categorytheory.Object](d, a, b, vertex), Iterable[Morphism](da, db, av, bv))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o da, bv o db))))
      .build()
    val functor1 = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> b, c ~> vertex, vertex ~> d),
      List[MorphismTransformation](va ~> da, vb ~> db, ac ~> av, bc ~> bv)
    )
    val limit = new Pullback(d, a, b, vertex, da, db, av, bv)
    val newLimits1 = limit.createPatternsInSourceCategory(functor1)
    assertResult(List(new Pullback(vertex, a, b, c, va, vb, ac, bc)))(newLimits1)

    val vertex2 = new Object("v2")
    val a2 = new Object("a2")
    val b2 = new Object("b2")
    val c2 = new Object("c2")
    val va2 = new Morphism("va2", vertex2, a2)
    val vb2 = new Morphism("vb2", vertex2, b2)
    val ac2 = new Morphism("ac2", a2, c2)
    val bc2 = new Morphism("bc2", b2, c2)
    val category3 = CategoryBuilder("c3", Iterable[categorytheory.Object](vertex, a, b, c, vertex2, a2, b2, c2), Iterable[Morphism](va, vb, ac, bc, va2, vb2, ac2, bc2))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb)), new MorphismEquality(Iterable(ac2 o va2, bc2 o vb2))))
      .build()
    val functor2 = new Functor("F", category3, category2,
      List[ObjectTransformation](a ~> a, b ~> b, c ~> vertex, vertex ~> d, a2 ~> a, b2 ~> b, c2 ~> vertex, vertex2 ~> d),
      List[MorphismTransformation](va ~> da, vb ~> db, ac ~> av, bc ~> bv, va2 ~> da, vb2 ~> db, ac2 ~> av, bc2 ~> bv)
    )
    val newLimits2 = limit.createPatternsInSourceCategory(functor2)
    assertResult(List(new Pullback(vertex, a, b, c, va, vb, ac, bc), new Pullback(vertex2, a2, b2, c2, va2, vb2, ac2, bc2)))(newLimits2)
  }

  test("isEqual returns correctly true") {
    assertResult(true)(new Pullback(vertex, a, b, c, va, vb, ac, bc) == new Pullback(vertex, a, b, c, va, vb, ac, bc))
  }

  test("isPreservedByFunctor returns true if the limit is preserved") {
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](va, vb, ac, bc))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb))))
      .build()
    val category2 = CategoryBuilder("c2", Iterable[categorytheory.Object](d, a, b, vertex), Iterable[Morphism](da, db, av, bv))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o da, bv o db))))
      .build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> b, c ~> vertex, vertex ~> d),
      List[MorphismTransformation](va ~> da, vb ~> db, ac ~> av, bc ~> bv)
    )
    val limit = new Pullback(vertex, a, b, c, va, vb, ac, bc)
    assertResult(true)(limit.isPreservedByFunctor(functor))
  }

  test("isPreservedByFunctor returns false if the limit is not preserved") {
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](va, vb, ac, bc))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb))))
      .build()
    val category2 = CategoryBuilder("c2", Iterable[categorytheory.Object](d, a, b, vertex), Iterable[Morphism](da, db, av, bv))
      .build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> b, c ~> vertex, vertex ~> d),
      List[MorphismTransformation](va ~> da, vb ~> db, ac ~> av, bc ~> bv)
    )
    val limit = new Pullback(vertex, a, b, c, va, vb, ac, bc)
    assertResult(false)(limit.isPreservedByFunctor(functor))
  }

  test("existsInSourceCategory returns a list of corresponding limits if they exist in the source category") {
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](va, vb, ac, bc))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ac o va, bc o vb))))
      .build()
    val category2 = CategoryBuilder("c2", Iterable[categorytheory.Object](d, a, b, vertex), Iterable[Morphism](da, db, av, bv))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o da, bv o db))))
      .build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> b, c ~> vertex, vertex ~> d),
      List[MorphismTransformation](va ~> da, vb ~> db, ac ~> av, bc ~> bv)
    )
    val limit = new Pullback(d, a, b, vertex, da, db, av, bv)
    assertResult(List(new Pullback(vertex, a, b, c, va, vb, ac, bc)))(limit.existsInSourceCategory(functor))
  }

  test("existsInSourceCategory returns an empty list if the limit does not exist in the source category") {
    val category1 = CategoryBuilder("c1", Iterable[categorytheory.Object](vertex, a, b, c), Iterable[Morphism](va, vb, ac, bc))
      .build()
    val category2 = CategoryBuilder("c2", Iterable[categorytheory.Object](d, a, b, vertex), Iterable[Morphism](da, db, av, bv))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o da, bv o db))))
      .build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> b, c ~> vertex, vertex ~> d),
      List[MorphismTransformation](va ~> da, vb ~> db, ac ~> av, bc ~> bv)
    )
    val limit = new Pullback(d, a, b, vertex, da, db, av, bv)
    assertResult(List())(limit.existsInSourceCategory(functor))
  }
}
