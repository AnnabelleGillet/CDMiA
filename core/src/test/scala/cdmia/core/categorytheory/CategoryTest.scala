package cdmia.core.categorytheory

import cdmia.core.categorytheory
import cdmia.core.categorytheory.morphism.{Isomorphism, Morphism, MorphismComposition, MorphismEquality}
import org.scalatest.funsuite.*

import java.lang

class CategoryTest extends AnyFunSuite {
  val a = new Object("a")
  val b = new Object("b")
  val c = new Object("c")
  val d = new Object("d")

  val mab = new Morphism("mab", a, b)
  val mab2 = new Morphism("mab2", a, b)
  val mad = new Morphism("mad", a, d)
  val mba = new Morphism("mba", b, a)
  val mba2 = new Morphism("mba2", b, a)
  val mba3 = new Morphism("mba3", b, a)
  val maa = new Morphism("maa", a, a)
  val mcb = new Morphism("mcb", c, b)
  val mcd = new Morphism("mcd", c, d)
  val mda = new Morphism("mda", d, a)
  val mdc = new Morphism("mdc", d, c)
  val mbc = new Morphism("mbc", b, c)
  val mac = new Morphism("mac", a, c)

  test("A category should produce the correct collection of identity morphisms from its objects") {
    assertResult(Seq[Morphism](a.identityMorphism))(CategoryBuilder("c", List[Object](a), List[Morphism]()).build().identityMorphisms)
    assertResult(Seq[Morphism](a.identityMorphism, b.identityMorphism))(categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism]()).build().identityMorphisms)
  }

  test("A category should produce the correct map of morphism equalities") {
    val equalities = List[MorphismEquality](
      new MorphismEquality(List[Morphism](mab, mab2)),
      new MorphismEquality(List[Morphism](mba, mba2, mba3))
    )
    val category = categorytheory.CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab, mba, maa, mab2, mba2, mba3))
      .withMorphismEqualities(equalities)
      .build()

    assertResult(6)(category.morphismEqualitiesForEachMorphism.size)
    assertResult(0)(category.morphismEqualitiesForEachComposition.size)

    assertResult(List[Morphism](maa, mab, mab2, mba, mba2, mba3).sortWith(_.name > _.name))(category.morphismEqualitiesForEachMorphism.keys.toList.sortWith(_.name > _.name))

    assertResult(List[Morphism](maa))(category.morphismEqualitiesForEachMorphism(maa).toList)
    assertResult(List[Morphism](mab, mab2).sortWith(_.name > _.name))(category.morphismEqualitiesForEachMorphism(mab).toList.sortWith(_.name > _.name))
    assertResult(List[Morphism](mab, mab2).sortWith(_.name > _.name))(category.morphismEqualitiesForEachMorphism(mab2).toList.sortWith(_.name > _.name))
    assertResult(List[Morphism](mba, mba2, mba3).sortWith(_.name > _.name))(category.morphismEqualitiesForEachMorphism(mba).toList.sortWith(_.name > _.name))
    assertResult(List[Morphism](mba, mba2, mba3).sortWith(_.name > _.name))(category.morphismEqualitiesForEachMorphism(mba2).toList.sortWith(_.name > _.name))
    assertResult(List[Morphism](mba, mba2, mba3).sortWith(_.name > _.name))(category.morphismEqualitiesForEachMorphism(mba3).toList.sortWith(_.name > _.name))
  }

  test("A category should produce the correct map of composition equalities") {
    val composition = mab.composeWith(mba)
    val category = CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab, mba, maa))
      .withMorphismEqualities(List[MorphismEquality](new MorphismEquality(List[Morphism](maa, composition))))
      .build()

    assertResult(3)(category.morphismEqualitiesForEachMorphism.size)
    assertResult(1)(category.morphismEqualitiesForEachComposition.size)

    assertResult(List[Morphism](composition))(category.morphismEqualitiesForEachComposition.map(_._1))

    assertResult(List[Morphism](composition, maa).sortWith(_.name > _.name))(category.morphismEqualitiesForEachComposition.filter(_._1 == composition).head._2.toList.sortWith(_.name > _.name))
  }

  test("existsMorphism should return true if at least one morphism exists between two objects.") {
    val category = categorytheory.CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab, mbc)).build()
    assertResult(true)(category.existsMorphism(a, a))
    assertResult(true)(category.existsMorphism(a, b))
    assertResult(true)(category.existsMorphism(a, c))
  }

  test("existsMorphism should throw an exception if an object is not in the category.") {
    val category = CategoryBuilder("c", List[Object](a, b), List[Morphism](mab)).build()
    assertThrows[IllegalArgumentException](category.existsMorphism(a, c))
    assertThrows[IllegalArgumentException](category.existsMorphism(c, a))
    assertThrows[IllegalArgumentException](category.existsMorphism(c, d))
  }

  test("existsMorphism should return false if no morphism exists between two objects.") {
    val category = CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab, mbc)).build()
    assertResult(false)(category.existsMorphism(a, d))
    assertResult(false)(category.existsMorphism(c, a))
    assertResult(false)(category.existsMorphism(b, a))
  }

  test("getMorphisms should return an empty list if no morphism exists between two objects.") {
    val category = categorytheory.CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab, mbc)).build()
    assertResult(List[Morphism]())(category.getMorphisms(a, d))
    assertResult(List[Morphism]())(category.getMorphisms(c, a))
    assertResult(List[Morphism]())(category.getMorphisms(b, a))
  }

  test("getMorphisms should throw an exception if an object is not in the category.") {
    val category = categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism](mab)).build()
    assertThrows[IllegalArgumentException](category.getMorphisms(a, c))
    assertThrows[IllegalArgumentException](category.getMorphisms(c, a))
    assertThrows[IllegalArgumentException](category.getMorphisms(c, d))
  }

  test("getMorphisms should return the correct list of morphisms existing between two objects.") {
    val category = CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab, mbc, mac)).build()
    assertResult(List[Morphism](a.identityMorphism))(category.getMorphisms(a, a))
    assertResult(List[Morphism](mab))(category.getMorphisms(a, b))
    assertResult(List[Morphism](mbc o mab, mac).sortWith(_.name > _.name))(category.getMorphisms(a, c).sortWith(_.name > _.name))
  }

  test("areEqual correctly returns true if the morphisms are equal") {
    val equalities = List[MorphismEquality](
      new MorphismEquality(List[Morphism](mab, mab2)),
      new MorphismEquality(List[Morphism](mba, mba2, mba3)),
      new MorphismEquality(List[Morphism](mab, mab2 o maa)),
    )
    val category = CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab, mba, maa, mab2, mba2, mba3))
      .withMorphismEqualities(equalities)
      .build()

    assertResult(true)(category.areEqual(mab, mab))
    assertResult(true)(category.areEqual(mab, mab o a.identityMorphism))
    assertResult(true)(category.areEqual(mab o a.identityMorphism, mab))
    assertResult(true)(category.areEqual(a.identityMorphism, a.identityMorphism))
    assertResult(true)(category.areEqual(a.identityMorphism o a.identityMorphism, a.identityMorphism))
    assertResult(true)(category.areEqual(a.identityMorphism o a.identityMorphism, a.identityMorphism o a.identityMorphism))
    assertResult(true)(category.areEqual(mab, mab2))
    assertResult(true)(category.areEqual(mab2, mab o a.identityMorphism))
    assertResult(true)(category.areEqual(mab o a.identityMorphism, mab2))
    assertResult(true)(category.areEqual(mab2 o maa o a.identityMorphism, mab))
    assertResult(true)(category.areEqual(mab, mab2 o maa o a.identityMorphism))
    assertResult(true)(category.areEqual(mab o a.identityMorphism, mab2 o maa o a.identityMorphism))
  }

  test("areEqual correctly returns true if the morphisms are equal by transitivity") {
    val equalities = List[MorphismEquality](
      new MorphismEquality(List[Morphism](mba, mba2)),
      new MorphismEquality(List[Morphism](mba2, mba3)),
      new MorphismEquality(List[Morphism](mba3, maa o mba2)),
    )
    val category = categorytheory.CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab, mba, maa, mab2, mba2, mba3))
      .withMorphismEqualities(equalities)
      .build()

    assertResult(true)(category.areEqual(mba, mba3))
    assertResult(true)(category.areEqual(mba, a.identityMorphism o mba3))
    assertResult(true)(category.areEqual(a.identityMorphism o mba, maa o mba2))
    assertResult(true)(category.areEqual(a.identityMorphism o maa o mba2, mba))
    assertResult(true)(category.areEqual(a.identityMorphism o maa o mba2, mba2))
    assertResult(true)(category.areEqual(mba, a.identityMorphism o maa o mba2))
    assertResult(true)(category.areEqual(a.identityMorphism o mba, a.identityMorphism o maa o mba2))
  }

  test("areEqual correctly returns true if the morphisms are equal and one morphism contains an isomorphism") {
    val equalities = List[MorphismEquality](
      new MorphismEquality(List[Morphism](mab, mab2))
    )
    val category = categorytheory.CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab, mba, mbc, mcb, mad, mda, maa, mab2, mba2, mba3))
      .withMorphismEqualities(equalities)
      .withIsomorphisms(List[Isomorphism](new Isomorphism(mbc, mcb), new Isomorphism(mad, mda)))
      .build()

    assertResult(true)(category.areEqual(mab2, mcb o mbc o mab))
    assertResult(true)(category.areEqual(mcb o mbc o mab2, mab))
    assertResult(true)(category.areEqual(mcb o mbc o mab2, mcb o mbc o mab))
    assertResult(true)(category.areEqual(mab2, mab o mda o mad))
    assertResult(true)(category.areEqual(mab2 o mda o mad, mab))
    assertResult(true)(category.areEqual(mab2 o mda o mad, mab o mda o mad))
  }

  test("areEqual correctly returns false if the morphisms are not equal") {
    val equalities = List[MorphismEquality](
      new MorphismEquality(List[Morphism](mba, mba2, mba3)),
      new MorphismEquality(List[Morphism](mab, mab2 o maa)),
    )
    val category = CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab, mba, maa, mab2, mba2, mba3))
      .withMorphismEqualities(equalities)
      .build()

    assertResult(false)(category.areEqual(mab, mba))
    assertResult(false)(category.areEqual(mba, mab o a.identityMorphism))
    assertResult(false)(category.areEqual(mab o a.identityMorphism, mba))
    assertResult(false)(category.areEqual(a.identityMorphism, b.identityMorphism))
    assertResult(false)(category.areEqual(mab, mab2))
    assertResult(false)(category.areEqual(mab2, mab o a.identityMorphism))
    assertResult(false)(category.areEqual(mab o a.identityMorphism, mab2))
    assertResult(false)(category.areEqual(mab2 o maa o a.identityMorphism, mab2))
    assertResult(false)(category.areEqual(mab2, mab2 o maa o a.identityMorphism))
    assertResult(false)(category.areEqual(mab2 o a.identityMorphism, mab2 o maa o a.identityMorphism))
  }

  test("isAnIsomorphism returns true if the two morphisms are an isomorphism") {
    val category = categorytheory.CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab, mba, maa, mab2, mba2, mba3))
      .withIsomorphisms(List[Isomorphism](new Isomorphism(mab, mba)))
      .build()

    assertResult(true)(category.isAnIsomorphism(a.identityMorphism, a.identityMorphism))
    assertResult(true)(category.isAnIsomorphism(mba, mab))
    assertResult(true)(category.isAnIsomorphism(mab, mba))
  }

  test("isAnIsomorphism returns true if the two compositions are an isomorphism") {
    val category = categorytheory.CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab, mba, mbc, mcb, maa, mab2, mba2, mba3))
      .withIsomorphisms(List[Isomorphism](new Isomorphism(mab, mba), new Isomorphism(mbc, mcb)))
      .build()

    assertResult(true)(category.isAnIsomorphism(mbc o mab, mba o mcb))
  }

  test("isAnIsomorphism returns false if the two morphisms are not an isomorphism") {
    val category = CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab, mba, maa, mab2, mba2, mba3))
      .withIsomorphisms(List[Isomorphism](new Isomorphism(mab, mba)))
      .build()

    assertResult(false)(category.isAnIsomorphism(a.identityMorphism, b.identityMorphism))
    assertResult(false)(category.isAnIsomorphism(mba, mab2))
    assertResult(false)(category.isAnIsomorphism(mab2, mba))
  }

  test("isAnIsomorphism should throw an exception is a morphism is not in the category") {
    val category = categorytheory.CategoryBuilder("c", List[Object](a, b, c), List[Morphism](mab, mba, maa, mba3))
      .withIsomorphisms(List[Isomorphism](new Isomorphism(mab, mba)))
      .build()

    assertThrows[IllegalArgumentException](category.isAnIsomorphism(d.identityMorphism, c.identityMorphism))
    assertThrows[IllegalArgumentException](category.isAnIsomorphism(c.identityMorphism, d.identityMorphism))
    assertThrows[IllegalArgumentException](category.isAnIsomorphism(d.identityMorphism, d.identityMorphism))
    assertThrows[IllegalArgumentException](category.isAnIsomorphism(mba, mab2))
    assertThrows[IllegalArgumentException](category.isAnIsomorphism(mba2, mab))
    assertThrows[IllegalArgumentException](category.isAnIsomorphism(mab2, mba2))
  }
}
