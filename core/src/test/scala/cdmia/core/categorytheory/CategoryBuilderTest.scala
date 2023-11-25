package cdmia.core.categorytheory

import cdmia.core.categorytheory
import cdmia.core.categorytheory.morphism.{Isomorphism, Morphism, MorphismEquality}
import cdmia.core.categorytheory.pattern.colimit.{Colimit, Coproduct, InitialObject}
import cdmia.core.categorytheory.pattern.limit.{Limit, Product, TerminalObject}
import org.scalatest.funsuite.*

import java.lang

class CategoryBuilderTest extends AnyFunSuite {
  val a = new Object("a")
  val b = new Object("b")
  val c = new Object("c")
  val d = new Object("d")

  val mab = new Morphism("mab", a, b)
  val mac = new Morphism("mac", a, c)
  val mad = new Morphism("mad", a, d)
  val mba = new Morphism("mba", b, a)
  val mbd = new Morphism("mbd", b, d)
  val mca = new Morphism("mca", c, a)
  val mcd = new Morphism("mcd", c, d)
  val mda = new Morphism("mda", d, a)
  val mdb = new Morphism("mdb", d, b)
  val mdc = new Morphism("mdc", d, c)

  test("A category should be built with empty collections of objects and morphisms") {
    assertResult(true)(CategoryBuilder("c", List[Object](), List[Morphism]()).build().isInstanceOf[Category])
    assertResult(true)(categorytheory.CategoryBuilder("c", Set[Object](), Set[Morphism]()).build().isInstanceOf[Category])
    assertResult(true)(categorytheory.CategoryBuilder("c", Array[Object](), Array[Morphism]()).build().isInstanceOf[Category])
  }

  /*
  Tests for objects
   */
  test("A category should be built with valid collection of objects") {
    assertResult(true)(CategoryBuilder("c", List[Object](a), List[Morphism]()).build().isInstanceOf[Category])
    assertResult(true)(CategoryBuilder("c", List[Object](a, b), List[Morphism]()).build().isInstanceOf[Category])
    assertResult(true)(categorytheory.CategoryBuilder("c", List[Object](a, b, c), List[Morphism]()).build().isInstanceOf[Category])
    assertResult(true)(CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism]()).build().isInstanceOf[Category])
  }

  test("A category should not be built with duplicate objects") {
    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, a), List[Morphism]()))
    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, b, b), List[Morphism]()))
    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, b, a), List[Morphism]()))
    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, a, b), List[Morphism]()))
  }

  /*
  Tests for morphisms
   */
  test("A category should be built with valid collection of morphisms") {
    assertResult(true)(categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism](mab)).build().isInstanceOf[Category])
    assertResult(true)(CategoryBuilder("c", List[Object](a, b), List[Morphism](mab, mba)).build().isInstanceOf[Category])
    assertResult(true)(CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab, mcd, mdc)).build().isInstanceOf[Category])
  }

  test("A category should not be built with duplicate morphisms") {
    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism](mab, mab)))
    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, b), List[Morphism](mab, mba, mba)))
    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism](mab, mab, mba)))
    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, b), List[Morphism](mab, mba, mab)))
  }

  test("A category should not be built with domain of morphisms not in collection of objects") {
    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, b, d), List[Morphism](mcd)))
    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, b, d), List[Morphism](mab, mcd)))
    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, b, d), List[Morphism](mab, mba, mcd)))
    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, b), List[Morphism](mcd, mdc)))
  }

  test("A category should not be built with codomain of morphisms not in collection of objects") {
    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, b, c), List[Morphism](mcd)))
    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, b, c), List[Morphism](mab, mcd)))
    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, b, c), List[Morphism](mab, mba, mcd)))
    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, b), List[Morphism](mcd, mdc)))
  }

  test("A category should not be built with identity morphisms in collection of morphisms") {
    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, b, d), List[Morphism](a.identityMorphism)))
    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, b, d), List[Morphism](mab, d.identityMorphism)))
    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, b, d), List[Morphism](a.identityMorphism, b.identityMorphism, mab)))
    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism](c.identityMorphism)))
  }

  /*
  Tests for morphism equalities
   */
  test("A category should be built with empty morphism equalities") {
    val equalities = List[MorphismEquality]()

    assertResult(true)(CategoryBuilder("c", List[Object](), List[Morphism]()).withMorphismEqualities(equalities).build().isInstanceOf[Category])
    assertResult(true)(CategoryBuilder("c", List[Object](a, b), List[Morphism]()).withMorphismEqualities(equalities).build().isInstanceOf[Category])
    assertResult(true)(categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism](mab)).withMorphismEqualities(equalities).build().isInstanceOf[Category])
  }

  test("A category should be built with valid morphism equalities") {
    val mab2 = new Morphism("mab2", a, b)
    val mab3 = new Morphism("mab2", a, b)
    val maa = new Morphism("maa", a, a)
    val mac = new Morphism("mcd2", a, c)
    val mcb = new Morphism("mdc2", c, b)
    val equalities1 = List[MorphismEquality](new MorphismEquality(List[Morphism](mab, mab2)))
    val equalities2 = List[MorphismEquality](new MorphismEquality(List[Morphism](mab, mab2, mab3)))

    assertResult(true)(categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism](mab, mab2)).withMorphismEqualities(equalities1).build().isInstanceOf[Category])
    assertResult(true)(CategoryBuilder("c", List[Object](a, b), List[Morphism](mab, mab2, mab3)).withMorphismEqualities(equalities2).build().isInstanceOf[Category])

    val composition1 = mba o mab
    val composition2 = mcb o mac o maa
    val equalities3 = List[MorphismEquality](new MorphismEquality(List[Morphism](composition1, maa)))
    val equalities4 = List[MorphismEquality](new MorphismEquality(List[Morphism](mab, composition2)))

    assertResult(true)(categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism](maa, mab, mba)).withMorphismEqualities(equalities3).build().isInstanceOf[Category])
    assertResult(true)(categorytheory.CategoryBuilder("c", List[Object](a, b, c), List[Morphism](maa, mab, mba, mac, mcb)).withMorphismEqualities(equalities4).build().isInstanceOf[Category])
  }

  test("A category should not be built if morphisms of morphism equalities are not in category") {
    val mab2 = new Morphism("mab2", a, b)
    val mab3 = new Morphism("mab2", a, b)
    val maa = new Morphism("maa", a, a)
    val mac = new Morphism("mcd2", a, c)
    val mcb = new Morphism("mdc2", c, b)
    val equalities1 = List[MorphismEquality](new MorphismEquality(List[Morphism](mab, mab2)))
    val equalities2 = List[MorphismEquality](new MorphismEquality(List[Morphism](mab, mab2, mab3)))

    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism](mab)).withMorphismEqualities(equalities1).build())
    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, b), List[Morphism](mab, mab3)).withMorphismEqualities(equalities2).build())

    val composition1 = mba o mab
    val composition2 = mcb o mac o maa
    val equalities3 = List[MorphismEquality](new MorphismEquality(List[Morphism](composition1, maa)))
    val equalities4 = List[MorphismEquality](new MorphismEquality(List[Morphism](mab, composition2)))

    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism](mab, mba)).withMorphismEqualities(equalities1).build())
    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, b, c), List[Morphism](maa, mab, mba, mcb)).withMorphismEqualities(equalities2).build())
  }

  /*
  Tests for isomorphisms
   */
  test("A category should be built with empty isomorphisms") {
    val isomorphisms = List[Isomorphism]()

    assertResult(true)(categorytheory.CategoryBuilder("c", List[Object](), List[Morphism]()).withIsomorphisms(isomorphisms).build().isInstanceOf[Category])
    assertResult(true)(categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism]()).withIsomorphisms(isomorphisms).build().isInstanceOf[Category])
    assertResult(true)(CategoryBuilder("c", List[Object](a, b), List[Morphism](mab)).withIsomorphisms(isomorphisms).build().isInstanceOf[Category])
  }

  test("A category should be built with valid isomorphisms") {
    val isomorphisms1 = List[Isomorphism](new Isomorphism(mab, mba))
    val isomorphisms2 = List[Isomorphism](new Isomorphism(mab, mba), new Isomorphism(mcd, mdc))

    assertResult(true)(CategoryBuilder("c", List[Object](a, b), List[Morphism](mab, mba)).withIsomorphisms(isomorphisms1).build().isInstanceOf[Category])
    assertResult(true)(categorytheory.CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab, mba, mcd, mdc)).withIsomorphisms(isomorphisms2).build().isInstanceOf[Category])

    val composition1 = mba o mab o mba
    val composition2 = mab o mba o mab
    val isomorphism3 = List[Isomorphism](new Isomorphism(composition1, composition2))

    assertResult(true)(CategoryBuilder("c", List[Object](a, b), List[Morphism](mab, mba)).withIsomorphisms(isomorphism3).build().isInstanceOf[Category])
  }

  test("A category should not be built if morphisms of isomorphisms are not in category") {
    val mab2 = new Morphism("mab2", a, b)
    val mba2 = new Morphism("mba2", b, a)
    val isomorphisms1 = List[Isomorphism](new Isomorphism(mab2, mba2))
    val isomorphisms2 = List[Isomorphism](new Isomorphism(mba2, mab2))

    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, b), List[Morphism](mab, mba)).withIsomorphisms(isomorphisms1).build())
    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism](mab, mba, mab2)).withIsomorphisms(isomorphisms2).build())

    val composition1 = mab2 o mba2 o mab2
    val composition2 = mba2 o mab2 o mba2
    val isomorphisms3 = List[Isomorphism](new Isomorphism(composition1, composition2))
    val isomorphisms4 = List[Isomorphism](new Isomorphism(composition2, composition1))

    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, b), List[Morphism](mab, mba)).withIsomorphisms(isomorphisms3).build())
    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, b, c), List[Morphism](mab, mba, mab2)).withIsomorphisms(isomorphisms4).build())
  }

  /*
  Tests for limits
   */
  test("A category should be built with empty limits") {
    val limits = List[Limit]()

    assertResult(true)(CategoryBuilder("c", List[Object](), List[Morphism]()).build(limits).isInstanceOf[Category])
    assertResult(true)(CategoryBuilder("c", List[Object](a, b), List[Morphism]()).build(limits).isInstanceOf[Category])
    assertResult(true)(categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism](mab)).build(limits).isInstanceOf[Category])
  }

  test("A category should be built with valid limits") {
    val limit1 = new TerminalObject(b)
    val limit2 = new Product(a, Iterable[Object](b, c), Iterable[Morphism](mab, mac))

    assertResult(true)(categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism](mab)).build(Iterable[Limit](limit1)).limits.exists(_ == limit1))
    assertResult(true)(CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab, mac)).build(Iterable[Limit](limit2)).limits.exists(_ == limit2))
  }

  test("A category should not be built with invalid limits") {
    val limit1 = new TerminalObject(d)
    val limit2 = new Product(a, Iterable[Object](b, c), Iterable[Morphism](mab, mac))

    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, b), List[Morphism](mab)).build(Iterable[Limit](limit1)))
    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab)).build(Iterable[Limit](limit2)))
  }

  test("A category should not be built if limits do not respect the universal property") {
    val limit1 = new TerminalObject(b)
    val limit2 = new Product(a, Iterable[Object](b, c), Iterable[Morphism](mab, mac))

    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism](mab, mba)).build(Iterable[Limit](limit1)))
    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mab, mac, mda, mdb)).build(Iterable[Limit](limit2)))
  }

  /*
  Tests for colimits
   */
  test("A category should be built with empty colimits") {
    val colimits = List[Colimit]()

    assertResult(true)(CategoryBuilder("c", List[Object](), List[Morphism]()).build(colimits = colimits).isInstanceOf[Category])
    assertResult(true)(categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism]()).build(colimits = colimits).isInstanceOf[Category])
    assertResult(true)(CategoryBuilder("c", List[Object](a, b), List[Morphism](mab)).build(colimits = colimits).isInstanceOf[Category])
  }

  test("A category should be built with valid colimits") {
    val colimit1 = new InitialObject(a)
    val colimit2 = new Coproduct(a, Iterable[Object](b, c), Iterable[Morphism](mba, mca))

    assertResult(true)(CategoryBuilder("c", List[Object](a, b), List[Morphism](mab)).build(colimits = Iterable[Colimit](colimit1)).colimits.exists(_ == colimit1))
    assertResult(true)(categorytheory.CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mba, mca)).build(colimits = Iterable[Colimit](colimit2)).colimits.exists(_ == colimit2))
  }

  test("A category should not be built with invalid colimits") {
    val colimit1 = new InitialObject(a)
    val colimit2 = new Coproduct(a, Iterable[Object](b, c), Iterable[Morphism](mba, mca))

    assertThrows[IllegalArgumentException](CategoryBuilder("c", List[Object](a, b), List[Morphism](mba)).build(colimits = Iterable[Colimit](colimit1)))
    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mba)).build(colimits = Iterable[Colimit](colimit2)))
  }

  test("A category should not be built if colimits do not respect the universal property") {
    val colimit1 = new InitialObject(a)
    val colimit2 = new Coproduct(a, Iterable[Object](b, c), Iterable[Morphism](mba, mca))

    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, b), List[Morphism](mab, mba)).build(colimits = Iterable[Colimit](colimit1)))
    assertThrows[IllegalArgumentException](categorytheory.CategoryBuilder("c", List[Object](a, b, c, d), List[Morphism](mba, mca, mad, mbd)).build(colimits = Iterable[Colimit](colimit2)))
  }
}
