package cdmia.core.categorytheory.checker

import cdmia.core.categorytheory.{CategoryBuilder, Object, ObjectTransformation}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.{Morphism, MorphismEquality, MorphismTransformation}
import org.scalatest.funsuite.*

class NaturalIsomorphismCheckerTest extends AnyFunSuite {
  val a = new Object("a")
  val b = new Object("b")
  val c = new Object("c")
  val d = new Object("d")

  val mab = new Morphism("mab", a, b)
  val mba = new Morphism("mba", b, a)
  val mcd = new Morphism("mcd", c, d)
  val mdc = new Morphism("mdc", d, c)

  val c1 = CategoryBuilder("c1", List[Object](a, b), List[Morphism](mab, mba)).build()
  val c2 = CategoryBuilder("c2", List[Object](b, c, d), List[Morphism](mcd, mdc)).build()

  val F1 = new Functor("F1", c1, c2,
    List[ObjectTransformation](a ~> c, b ~> d),
    List[MorphismTransformation](mab ~> mcd, mba ~> mdc)
  )

  val F2 = new Functor("F2", c1, c2,
    List[ObjectTransformation](a ~> b, b ~> b),
    List[MorphismTransformation](mab ~> b.identityMorphism, mba ~> b.identityMorphism)
  )

  test("A NaturalIsomorphismChecker should be built with two functors having the same categories as domain and codomain") {
    assertResult(true)(new NaturalIsomorphismChecker(F1, F2).isInstanceOf[NaturalIsomorphismChecker])
  }

  test("A NaturalIsomorphismChecker should not be built with two functors different categories as domain and codomain") {
    val F3 = new Functor("F3", c1, c1,
      List[ObjectTransformation](a ~> a, b ~> b),
      List[MorphismTransformation](mab ~> mab, mba ~> mba)
    )
    val F4 = new Functor("F4", c2, c2,
      List[ObjectTransformation](b ~> b, c ~> c, d ~> d),
      List[MorphismTransformation](mcd ~> mcd, mdc ~> mdc)
    )
    val F5 = new Functor("F5", c2, c1,
      List[ObjectTransformation](b ~> a, c ~> a, d ~> a),
      List[MorphismTransformation](mcd ~> a.identityMorphism, mdc ~> a.identityMorphism)
    )

    assertThrows[IllegalArgumentException](new NaturalIsomorphismChecker(F1, F3))
    assertThrows[IllegalArgumentException](new NaturalIsomorphismChecker(F1, F4))
    assertThrows[IllegalArgumentException](new NaturalIsomorphismChecker(F1, F5))
  }

  test("isValid should return true when checking a functor with itself") {
    assertResult(true)(new NaturalIsomorphismChecker(F1, F1).isValid)
    assertResult(true)(new NaturalIsomorphismChecker(F2, F2).isValid)
  }

  test("isValid should return false when there is no natural isomorphism") {
    assertResult(false)(new NaturalIsomorphismChecker(F1, F2).isValid)
    assertResult(false)(new NaturalIsomorphismChecker(F2, F1).isValid)
  }

  test("isValid should return false is there is a natural transformation but not a natural isomorphism") {
    val F3 = new Functor("F3", c1, c2,
      List[ObjectTransformation](a ~> d, b ~> c),
      List[MorphismTransformation](mab ~> mdc, mba ~> mcd)
    )
    assertResult(false)(new NaturalIsomorphismChecker(F1, F3).isValid)
    assertResult(false)(new NaturalIsomorphismChecker(F3, F1).isValid)
  }

  test("isValid should return true is there is a natural isomorphism between two different functors") {
    val mcd2 = new Morphism("mcd2", c, d)
    val c3 = CategoryBuilder("c3", List[Object](a, b, c, d), List[Morphism](mcd, mcd2, mdc))
      .withMorphismEqualities(List[MorphismEquality](new MorphismEquality(List[Morphism](mcd, mcd2)))).build()

    val F3 = new Functor("F3", c1, c3,
      List[ObjectTransformation](a ~> d, b ~> c),
      List[MorphismTransformation](mab ~> mdc, mba ~> mcd)
    )
    val F4 = new Functor("F4", c1, c3,
      List[ObjectTransformation](a ~> d, b ~> c),
      List[MorphismTransformation](mab ~> mdc, mba ~> mcd2)
    )
    assertResult(true)(new NaturalTransformationChecker(F3, F4).isValid)
    assertResult(true)(new NaturalTransformationChecker(F4, F3).isValid)
  }
}
