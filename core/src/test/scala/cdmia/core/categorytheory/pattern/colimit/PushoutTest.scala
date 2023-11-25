package cdmia.core.categorytheory.pattern.colimit

import cdmia.core.categorytheory.{CategoryBuilder, Object, ObjectTransformation}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.{Isomorphism, Morphism, MorphismEquality, MorphismTransformation}
import org.scalatest.funsuite.AnyFunSuite

class PushoutTest extends AnyFunSuite {
  val a = new Object("a")
  val b = new Object("b")
  val c = new Object("c")
  val d = new Object("d")
  val vertex = new Object("vertex")

  val ab = new Morphism("ab", a, b)
  val ac = new Morphism("ac", a, c)
  val ad = new Morphism("ad", a, d)
  val ba = new Morphism("ba", b, a)
  val bc = new Morphism("bc", b, c)
  val bd = new Morphism("bd", b, d)
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

  test("A pushout can be built") {
    assertResult(true)(new Pushout(vertex, a, b, c, av, bv, ca, cb).isInstanceOf[Pushout])
  }

  test("getMorphisms returns a list with the morphisms") {
    assertResult(List[Morphism](av, bv, ca, cb))(new Pushout(vertex, a, b, c, av, bv, ca, cb).getMorphisms)
  }

  test("getObjects returns a list with the objects") {
    assertResult(List[Object](vertex, a, b, c))(new Pushout(vertex, a, b, c, av, bv, ca, cb).getObjects)
  }

  test("A pushout cannot be built when va does not match vertex and a") {
    assertThrows[IllegalArgumentException](new Pushout(vertex, a, b, c, va, bv, ca, cb))
  }

  test("A pushout cannot be built when vb does not match vertex and b") {
    assertThrows[IllegalArgumentException](new Pushout(vertex, a, b, c, av, vb, ca, cb))
  }

  test("A pushout cannot be built when ac does not match a and c") {
    assertThrows[IllegalArgumentException](new Pushout(vertex, a, b, c, av, bv, ac, cb))
  }

  test("A pushout cannot be built when bc does not match b and c") {
    assertThrows[IllegalArgumentException](new Pushout(vertex, a, b, c, av, bv, ca, bc))
  }

  test("A pushout is valid in a category") {
    val category = CategoryBuilder("c", Iterable[Object](vertex, a, b, c), Iterable[Morphism](av, bv, ca, cb))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb))))
      .build()
    val pushout = new Pushout(vertex, a, b, c, av, bv, ca, cb)
    assertResult(true)(pushout.isValid(category))
  }

  test("A pushout is valid in a category with other objects and morphisms") {
    val category = CategoryBuilder("c", Iterable[Object](vertex, a, b, c, d), Iterable[Morphism](av, bv, ca, cb, cd))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb))))
      .build()
    val pushout = new Pushout(vertex, a, b, c, av, bv, ca, cb)
    assertResult(true)(pushout.isValid(category))
  }

  test("A pushout is not valid in a category when some objects are missing") {
    val category = CategoryBuilder("c", Iterable[Object](vertex, a, b), Iterable[Morphism](av, bv))
      .build()
    val pushout = new Pushout(vertex, a, b, c, av, bv, ca, cb)
    assertResult(false)(pushout.isValid(category))
  }

  test("A pushout is not valid in a category when the diagram does not commute") {
    val category = CategoryBuilder("c", Iterable[Object](vertex, a, b, c), Iterable[Morphism](av, bv, ca, cb))
      .build()
    val pushout = new Pushout(vertex, a, b, c, av, bv, ca, cb)
    assertResult(false)(pushout.isValid(category))
  }

  test("explainIsNotValid returns an empty list if the colimit is valid") {
    val category = CategoryBuilder("c", Iterable[Object](vertex, a, b, c), Iterable[Morphism](av, bv, ca, cb))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb))))
      .build()
    val pushout = new Pushout(vertex, a, b, c, av, bv, ca, cb)
    assertResult(List[String]())(pushout.explainIsNotValid(category))
  }

  test("explainIsNotValid returns a non-empty list if the colimit is not valid") {
    val category = CategoryBuilder("c", Iterable[Object](vertex, a, b, c), Iterable[Morphism](av, bv, ca, cb))
      .build()
    val pushout = new Pushout(vertex, a, b, c, av, bv, ca, cb)
    assertResult(1)(pushout.explainIsNotValid(category).size)
  }

  test("A pushout respects the universal property when no other object can be vertex") {
    val category = CategoryBuilder("c", Iterable[Object](vertex, a, b, c), Iterable[Morphism](av, bv, ca, cb))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb))))
      .build()
    val pushout = new Pushout(vertex, a, b, c, av, bv, ca, cb)
    assertResult(true)(pushout.respectsUniversalProperty(category))
  }

  test("A pushout does not respect the universal property when another object can be vertex") {
    val category = CategoryBuilder("c", Iterable[Object](vertex, a, b, c, d), Iterable[Morphism](av, bv, ca, cb, ad, bd))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb)), new MorphismEquality(Iterable(ad o ca, bd o cb))))
      .build()
    val pushout = new Pushout(vertex, a, b, c, av, bv, ca, cb)
    assertResult(false)(pushout.respectsUniversalProperty(category))
  }

  test("A pushout respects the universal property when the vertex is domain and the diagram commutes") {
    val category = CategoryBuilder("c", Iterable[Object](vertex, a, b, c, d), Iterable[Morphism](av, bv, ca, cb, ad, bd, vd))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb)), new MorphismEquality(Iterable(ad o ca, bd o cb)),
        new MorphismEquality(Iterable(ad, vd o av)), new MorphismEquality(Iterable(bd, vd o bv))))
      .build()
    val pushout = new Pushout(vertex, a, b, c, av, bv, ca, cb)
    assertResult(true)(pushout.respectsUniversalProperty(category))
  }

  test("A pushout respects the universal property when the vertex is domain and the diagram commutes, the other object having different morphisms than those of the pushout") {
    val category = CategoryBuilder("c", Iterable[Object](vertex, a, b, c, d), Iterable[Morphism](av, bv, ca, cb, cd, vd))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb))))
      .build()
    val pushout = new Pushout(vertex, a, b, c, av, bv, ca, cb)
    assertResult(true)(pushout.respectsUniversalProperty(category))
  }

  test("A pushout does not respect the universal property when the vertex is domain and the diagram does not commute, the other object having only one morphism towards an object of the pushout") {
    val category = CategoryBuilder("c", Iterable[Object](vertex, a, b, c, d), Iterable[Morphism](av, bv, ca, cb, ad, cd, vd))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb))))
      .build()
    val pushout = new Pushout(vertex, a, b, c, av, bv, ca, cb)
    assertResult(false)(pushout.respectsUniversalProperty(category))
  }

  test("A pushout does not respect the universal property when the vertex is domain and the diagram does not commute") {
    val category1 = CategoryBuilder("c", Iterable[Object](vertex, a, b, c, d), Iterable[Morphism](av, bv, ca, cb, ad, bd, vd))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb)), new MorphismEquality(Iterable(ad o ca, bd o cb)),
        new MorphismEquality(Iterable(ad, vd o av))))
      .build()
    val category2 = CategoryBuilder("c", Iterable[Object](vertex, a, b, c, d), Iterable[Morphism](av, bv, ca, cb, ad, bd, vd))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb)), new MorphismEquality(Iterable(ad o ca, bd o cb)),
        new MorphismEquality(Iterable(bd, vd o bv))))
      .build()
    val category3 = CategoryBuilder("c", Iterable[Object](vertex, a, b, c, d), Iterable[Morphism](av, bv, ca, cb, ad, bd, vd))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb)), new MorphismEquality(Iterable(ad o ca, bd o cb))))
      .build()
    val pushout = new Pushout(vertex, a, b, c, av, bv, ca, cb)
    assertResult(false)(pushout.respectsUniversalProperty(category1))
    assertResult(false)(pushout.respectsUniversalProperty(category2))
    assertResult(false)(pushout.respectsUniversalProperty(category3))
  }

  test("A pushout does not respect the universal property when the vertex is codomain and the diagram commute") {
    val category = CategoryBuilder("c", Iterable[Object](vertex, a, b, c, d), Iterable[Morphism](av, bv, ca, cb, ad, bd, vd))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb)), new MorphismEquality(Iterable(ad o ca, bd o cb)),
        new MorphismEquality(Iterable(ad, vd o av)), new MorphismEquality(Iterable(bd, vd o bv))))
      .build()
    val pushout = new Pushout(d, a, b, c, ad, bd, ca, cb)
    assertResult(false)(pushout.respectsUniversalProperty(category))
  }

  test("A pushout respects the universal property if it is unique up to isomorphisms") {
    val category = CategoryBuilder("c", Iterable[Object](vertex, a, b, c, d), Iterable[Morphism](av, bv, ca, cb, ad, bd, vd, dv))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb)), new MorphismEquality(Iterable(ad o ca, bd o cb)),
        new MorphismEquality(Iterable(ad, vd o av)), new MorphismEquality(Iterable(bd, vd o bv)),
        new MorphismEquality(Iterable(av, dv o ad)), new MorphismEquality(Iterable(bv, dv o bd))))
      .withIsomorphisms(Iterable[Isomorphism](new Isomorphism(dv, vd)))
      .build()
    val pushout1 = new Pushout(vertex, a, b, c, av, bv, ca, cb)
    val pushout2 = new Pushout(d, a, b, c, ad, bd, ca, cb)
    assertResult(true)(pushout1.respectsUniversalProperty(category))
    assertResult(true)(pushout2.respectsUniversalProperty(category))
  }

  test("A pushout does not respect the universal property if it is not unique up to isomorphisms") {
    val category = CategoryBuilder("c", Iterable[Object](vertex, a, b, c, d), Iterable[Morphism](av, bv, ca, cb, ad, bd, vd, dv))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb)), new MorphismEquality(Iterable(ad o ca, bd o cb)),
        new MorphismEquality(Iterable(ad, vd o av)), new MorphismEquality(Iterable(bd, vd o bv))))
      .build()
    val pushout1 = new Pushout(vertex, a, b, c, av, bv, ca, cb)
    val pushout2 = new Pushout(d, a, b, c, ad, bd, ca, cb)
    assertResult(false)(pushout1.respectsUniversalProperty(category))
    assertResult(false)(pushout2.respectsUniversalProperty(category))
  }

  test("createColimitInDestinationCategory correctly builds the colimit") {
    val category1 = CategoryBuilder("c1", Iterable[Object](vertex, a, b, c), Iterable[Morphism](av, bv, ca, cb))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb))))
      .build()
    val category2 = CategoryBuilder("c2", Iterable[Object](d, a, b, vertex), Iterable[Morphism](ad, bd, va, vb))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ad o va, bd o vb))))
      .build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> b, c ~> vertex, vertex ~> d),
      List[MorphismTransformation](av ~> ad, bv ~> bd, ca ~> va, cb ~> vb)
    )
    val colimit = new Pushout(vertex, a, b, c, av, bv, ca, cb)
    val newColimit = colimit.createPatternInDestinationCategory(functor).asInstanceOf[Pushout]
    assertResult(d)(newColimit.vertex)
    assertResult(a)(newColimit.a)
    assertResult(b)(newColimit.b)
    assertResult(vertex)(newColimit.c)
    assertResult(ad)(newColimit.av)
    assertResult(bd)(newColimit.bv)
    assertResult(va)(newColimit.ca)
    assertResult(vb)(newColimit.cb)
  }

  test("createColimitsInSourceCategory correctly builds the colimit") {
    val category1 = CategoryBuilder("c1", Iterable[Object](vertex, a, b, c), Iterable[Morphism](av, bv, ca, cb))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb))))
      .build()
    val category2 = CategoryBuilder("c2", Iterable[Object](d, a, b, vertex), Iterable[Morphism](ad, bd, va, vb))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ad o va, bd o vb))))
      .build()
    val functor1 = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> b, c ~> vertex, vertex ~> d),
      List[MorphismTransformation](av ~> ad, bv ~> bd, ca ~> va, cb ~> vb)
    )
    val colimit = new Pushout(d, a, b, vertex, ad, bd, va, vb)
    val newColimits1 = colimit.createPatternsInSourceCategory(functor1)
    assertResult(List(new Pushout(vertex, a, b, c, av, bv, ca, cb)))(newColimits1)

    val vertex2 = new Object("v2")
    val a2 = new Object("a2")
    val b2 = new Object("b2")
    val c2 = new Object("c2")
    val av2 = new Morphism("av2", a2, vertex2)
    val bv2 = new Morphism("bv2", b2, vertex2)
    val ca2 = new Morphism("ca2", c2, a2)
    val cb2 = new Morphism("cb2", c2, b2)
    val category3 = CategoryBuilder("c3", Iterable[Object](vertex, a, b, c, vertex2, a2, b2, c2), Iterable[Morphism](av, bv, ca, cb, av2, bv2, ca2, cb2))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb)), new MorphismEquality(Iterable(av2 o ca2, bv2 o cb2))))
      .build()
    val functor2 = new Functor("F", category3, category2,
      List[ObjectTransformation](a ~> a, b ~> b, c ~> vertex, vertex ~> d, a2 ~> a, b2 ~> b, c2 ~> vertex, vertex2 ~> d),
      List[MorphismTransformation](av ~> ad, bv ~> bd, ca ~> va, cb ~> vb, av2 ~> ad, bv2 ~> bd, ca2 ~> va, cb2 ~> vb)
    )
    val newColimits2 = colimit.createPatternsInSourceCategory(functor2)
    assertResult(List(new Pushout(vertex, a, b, c, av, bv, ca, cb), new Pushout(vertex2, a2, b2, c2, av2, bv2, ca2, cb2)))(newColimits2)
  }

  test("isEqual returns correctly true") {
    assertResult(true)(new Pushout(vertex, a, b, c, av, bv, ca, cb) == new Pushout(vertex, a, b, c, av, bv, ca, cb))
  }

  test("isPreservedByFunctor returns true if the colimit is preserved") {
    val category1 = CategoryBuilder("c1", Iterable[Object](vertex, a, b, c), Iterable[Morphism](av, bv, ca, cb))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb))))
      .build()
    val category2 = CategoryBuilder("c2", Iterable[Object](d, a, b, vertex), Iterable[Morphism](ad, bd, va, vb))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ad o va, bd o vb))))
      .build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> b, c ~> vertex, vertex ~> d),
      List[MorphismTransformation](av ~> ad, bv ~> bd, ca ~> va, cb ~> vb)
    )
    val colimit = new Pushout(vertex, a, b, c, av, bv, ca, cb)
    assertResult(true)(colimit.isPreservedByFunctor(functor))
  }

  test("isPreservedByFunctor returns false if the colimit is not preserved") {
    val category1 = CategoryBuilder("c1", Iterable[Object](vertex, a, b, c), Iterable[Morphism](av, bv, ca, cb))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb))))
      .build()
    val category2 = CategoryBuilder("c2", Iterable[Object](d, a, b, vertex), Iterable[Morphism](ad, bd, va, vb))
      .build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> b, c ~> vertex, vertex ~> d),
      List[MorphismTransformation](av ~> ad, bv ~> bd, ca ~> va, cb ~> vb)
    )
    val colimit = new Pushout(vertex, a, b, c, av, bv, ca, cb)
    assertResult(false)(colimit.isPreservedByFunctor(functor))
  }

  test("existsInSourceCategory returns a list of corresponding colimits if they exist in the source category") {
    val category1 = CategoryBuilder("c1", Iterable[Object](vertex, a, b, c), Iterable[Morphism](av, bv, ca, cb))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(av o ca, bv o cb))))
      .build()
    val category2 = CategoryBuilder("c2", Iterable[Object](d, a, b, vertex), Iterable[Morphism](ad, bd, va, vb))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ad o va, bd o vb))))
      .build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> b, c ~> vertex, vertex ~> d),
      List[MorphismTransformation](av ~> ad, bv ~> bd, ca ~> va, cb ~> vb)
    )
    val colimit = new Pushout(d, a, b, vertex, ad, bd, va, vb)
    assertResult(List(new Pushout(vertex, a, b, c, av, bv, ca, cb)))(colimit.existsInSourceCategory(functor))
  }

  test("existsInSourceCategory returns an empty list if the colimit does not exist in the source category") {
    val category1 = CategoryBuilder("c1", Iterable[Object](vertex, a, b, c), Iterable[Morphism](av, bv, ca, cb))
      .build()
    val category2 = CategoryBuilder("c2", Iterable[Object](d, a, b, vertex), Iterable[Morphism](ad, bd, va, vb))
      .withMorphismEqualities(Iterable(new MorphismEquality(Iterable(ad o va, bd o vb))))
      .build()
    val functor = new Functor("F", category1, category2,
      List[ObjectTransformation](a ~> a, b ~> b, c ~> vertex, vertex ~> d),
      List[MorphismTransformation](av ~> ad, bv ~> bd, ca ~> va, cb ~> vb)
    )
    val colimit = new Pushout(d, a, b, vertex, ad, bd, va, vb)
    assertResult(List())(colimit.existsInSourceCategory(functor))
  }
}
