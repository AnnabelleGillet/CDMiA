package cdmia.core.categorytheory.functor

import cdmia.core.categorytheory.{CategoryBuilder, Object, ObjectTransformation}
import cdmia.core.categorytheory.morphism.{Morphism, MorphismEquality, MorphismTransformation}
import org.scalatest.funsuite.*

class FunctorTest extends AnyFunSuite {
  val a = new Object("a")
  val b = new Object("b")
  val c = new Object("c")
  val d = new Object("d")

  val mab = new Morphism("mab", a, b)
  val mab2 = new Morphism("mab2", a, b)
  val mba = new Morphism("mba", b, a)
  val mcd = new Morphism("mcd", c, d)
  val mcd2 = new Morphism("mcd2", c, d)
  val mdc = new Morphism("mdc", d, c)

  val c1 = CategoryBuilder("c1", List[Object](a, b), List[Morphism](mab, mba)).build()
  val c2 = CategoryBuilder("c2", List[Object](b, c, d), List[Morphism](mcd, mdc)).build()

  test("A functor should be built with empty categories") {
    assertResult(true)(new Functor("F",
      CategoryBuilder("c1", List[Object](), List[Morphism]()).build(),
      CategoryBuilder("c1", List[Object](), List[Morphism]()).build(),
      List[ObjectTransformation](),
      List[MorphismTransformation]()).isInstanceOf[Functor])
  }

  test("A functor should be built with empty source category") {
    assertResult(true)(new Functor("F",
      CategoryBuilder("c1", List[Object](), List[Morphism]()).build(),
      CategoryBuilder("c1", List[Object](a, b), List[Morphism]()).build(),
      List[ObjectTransformation](),
      List[MorphismTransformation]()).isInstanceOf[Functor])

    assertResult(true)(new Functor("F",
      CategoryBuilder("c1", List[Object](), List[Morphism]()).build(),
      CategoryBuilder("c1", List[Object](a, b), List[Morphism](mab, mba)).build(),
      List[ObjectTransformation](),
      List[MorphismTransformation]()).isInstanceOf[Functor])
  }

  test("A functor should be built with a source category with only objects") {
    assertResult(true)(new Functor("F",
      CategoryBuilder("c1", List[Object](a, b), List[Morphism]()).build(),
      CategoryBuilder("c1", List[Object](c, d), List[Morphism]()).build(),
      List[ObjectTransformation](a ~> c, b ~> d),
      List[MorphismTransformation]()).isInstanceOf[Functor])

    assertResult(true)(new Functor("F",
      CategoryBuilder("c1", List[Object](a), List[Morphism]()).build(),
      CategoryBuilder("c1", List[Object](a, b), List[Morphism](mab, mba)).build(),
      List[ObjectTransformation](a ~> a),
      List[MorphismTransformation]()).isInstanceOf[Functor])
  }

  test("A functor should not be built with a source category with only objects and invalid transformations") {
    assertThrows[IllegalArgumentException](new Functor("F",
      CategoryBuilder("c1", List[Object](a, b), List[Morphism]()).build(),
      CategoryBuilder("c1", List[Object](c, d), List[Morphism]()).build(),
      List[ObjectTransformation](),
      List[MorphismTransformation]()))

    assertThrows[IllegalArgumentException](new Functor("F",
      CategoryBuilder("c1", List[Object](a, b), List[Morphism]()).build(),
      CategoryBuilder("c1", List[Object](c, d), List[Morphism]()).build(),
      List[ObjectTransformation](a ~> c),
      List[MorphismTransformation]()))

    assertThrows[IllegalArgumentException](new Functor("F",
      CategoryBuilder("c1", List[Object](a), List[Morphism]()).build(),
      CategoryBuilder("c1", List[Object](a, b), List[Morphism](mab, mba)).build(),
      List[ObjectTransformation](b ~> a),
      List[MorphismTransformation]()))
  }

  test("A functor should be built with a source category with objects and morphisms") {
    assertResult(true)(new Functor("F",
      c1,
      c2,
      List[ObjectTransformation](a ~> c, b ~> c),
      List[MorphismTransformation](mab ~> c.identityMorphism, mba ~> c.identityMorphism)).isInstanceOf[Functor])

    assertResult(true)(new Functor("F",
      c1,
      c2,
      List[ObjectTransformation](a ~> c, b ~> d),
      List[MorphismTransformation](mab ~> mcd, mba ~> mdc)).isInstanceOf[Functor])
  }

  test("A functor should not be built with a source category with objects and morphisms and invalid transformations") {
    assertThrows[IllegalArgumentException](new Functor("F",
      c1,
      c2,
      List[ObjectTransformation](a ~> a, b ~> c),
      List[MorphismTransformation](mab ~> c.identityMorphism, mba ~> c.identityMorphism)))

    assertThrows[IllegalArgumentException](new Functor("F",
      c1,
      c2,
      List[ObjectTransformation](a ~> c, b ~> d),
      List[MorphismTransformation](mab ~> mab, mba ~> mdc)).isInstanceOf[Functor])

    assertThrows[IllegalArgumentException](new Functor("F",
      c1,
      c2,
      List[ObjectTransformation](a ~> c, b ~> d),
      List[MorphismTransformation](mab ~> mdc, mba ~> mdc)).isInstanceOf[Functor])
  }

  test("A functor should not be built with identity morphism transformations") {
    assertThrows[IllegalArgumentException](new Functor("F",
      c1,
      c2,
      List[ObjectTransformation](a ~> c, b ~> c),
      List[MorphismTransformation](mab ~> c.identityMorphism, mba ~> c.identityMorphism, a.identityMorphism ~> c.identityMorphism)))
  }

  test("A functor should respect the functorial law when there is no morphism equality") {
    assertResult(true)(new Functor("F",
      c1,
      c2,
      List[ObjectTransformation](a ~> c, b ~> c),
      List[MorphismTransformation](mab ~> c.identityMorphism, mba ~> c.identityMorphism)).respectFunctorialLaw)

    assertResult(true)(new Functor("F",
      c1,
      c2,
      List[ObjectTransformation](a ~> c, b ~> d),
      List[MorphismTransformation](mab ~> mcd, mba ~> mdc)).respectFunctorialLaw)
  }

  test("A functor should respect the functorial law when morphism equalities are preserved") {
    val c1 = CategoryBuilder("c1", List[Object](a, b), List[Morphism](mab, mab2, mba))
      .withMorphismEqualities(List(new MorphismEquality(List(mab, mab2))))
      .build()
    val c2 = CategoryBuilder("c2", List[Object](b, c, d), List[Morphism](mcd, mcd2, mdc))
      .withMorphismEqualities(List(new MorphismEquality(List(mcd, mcd2))))
      .build()
    val functor = new Functor("F",
      c1,
      c2,
      List[ObjectTransformation](a ~> c, b ~> d),
      List[MorphismTransformation](mab ~> mcd, mab2 ~> mcd2, mba ~> mdc))

    assertResult(true)(functor.respectFunctorialLaw)
  }

  test("A functor should not respect the functorial law when morphism equalities are not preserved") {
    val c1 = CategoryBuilder("c1", List[Object](a, b), List[Morphism](mab, mab2, mba))
      .withMorphismEqualities(List(new MorphismEquality(List(mab, mab2))))
      .build()
    val c2 = CategoryBuilder("c2", List[Object](b, c, d), List[Morphism](mcd, mcd2, mdc))
      .build()
    val functor = new Functor("F",
      c1,
      c2,
      List[ObjectTransformation](a ~> c, b ~> d),
      List[MorphismTransformation](mab ~> mcd, mab2 ~> mcd2, mba ~> mdc))

    assertResult(false)(functor.respectFunctorialLaw)
  }

  test("A composition of functors should produce the correct result") {
    val c3 = CategoryBuilder("c3", List[Object](b, c, d), List[Morphism](mcd, mdc)).build()
    val f1 = new Functor("F",
      c1,
      c2,
      List[ObjectTransformation](a ~> c, b ~> d),
      List[MorphismTransformation](mab ~> mcd, mba ~> mdc)
    )
    val f2 = new Functor("F",
      c2,
      c3,
      List[ObjectTransformation](b ~> b, c ~> d, d ~> c),
      List[MorphismTransformation](mcd ~> mdc, mdc ~> mcd)
    )

    assertResult(true)(f1.composeWith(f2).isInstanceOf[Functor])
    assertResult(true)((f2 o f1).isInstanceOf[Functor])
    assertResult(true)((f2 ○ f1).isInstanceOf[Functor])

    val composition = f2 o f1

    assertResult(d)(composition.getDestinationObject(a))
    assertResult(c)(composition.getDestinationObject(b))
    assertResult(mdc)(composition.getDestinationMorphism(mab))
    assertResult(mcd)(composition.getDestinationMorphism(mba))
  }

  test("A composition of functors should throw an exception if the domain and codomain are not the same") {
    val c3 = CategoryBuilder("c3", List[Object](b, c, d), List[Morphism](mcd, mdc)).build()
    val f1 = new Functor("F",
      c1,
      c2,
      List[ObjectTransformation](a ~> c, b ~> d),
      List[MorphismTransformation](mab ~> mcd, mba ~> mdc)
    )
    val f2 = new Functor("F",
      c2,
      c3,
      List[ObjectTransformation](b ~> b, c ~> c, d ~> d),
      List[MorphismTransformation](mcd ~> mcd, mdc ~> mdc)
    )

    assertThrows[IllegalArgumentException](f1 o f2)
  }

  test("A functor should return the right result with toString") {
    assertResult(s"Functor(F: Category(c1) -> Category(c2))")(new Functor("F",
      c1,
      c2,
      List[ObjectTransformation](a ~> c, b ~> d),
      List[MorphismTransformation](mab ~> mcd, mba ~> mdc)).toString)
  }
}
