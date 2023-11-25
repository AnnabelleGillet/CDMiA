package cdmia.core.categorytheory

import org.scalatest.funsuite._

class ObjectTransformationTest extends AnyFunSuite {
  test("An ObjectTransformation should be correctly built") {
    val a = new Object("a")
    val b = new Object("b")

    assertResult(true)(new ObjectTransformation(a, b).isInstanceOf[ObjectTransformation])
  }

  test("An ObjectTransformation should produce the correct identity morphism transformation") {
    val a = new Object("a")
    val b = new Object("b")

    val imt1 = new ObjectTransformation(a, b).identityMorphismTransformation

    assertResult(true)(imt1.source == a.identityMorphism)
    assertResult(true)(imt1.destination == b.identityMorphism)

    val imt2 = new ObjectTransformation(a, a).identityMorphismTransformation

    assertResult(true)(imt2.source == a.identityMorphism)
    assertResult(true)(imt2.destination == a.identityMorphism)
  }
}
