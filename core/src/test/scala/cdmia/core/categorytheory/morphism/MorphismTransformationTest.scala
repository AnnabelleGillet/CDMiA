package cdmia.core.categorytheory.morphism

import cdmia.core.categorytheory.Object
import org.scalatest.funsuite.*

class MorphismTransformationTest extends AnyFunSuite {
  test("A MorphismTransformation should be correctly built") {
    val a = new Object("a")
    val b = new Object("b")
    val m1 = new Morphism("m1", a, b)
    val m2 = new Morphism("m2", b, a)

    assertResult(true)(new MorphismTransformation(m1, m2).isInstanceOf[MorphismTransformation])
  }
}
