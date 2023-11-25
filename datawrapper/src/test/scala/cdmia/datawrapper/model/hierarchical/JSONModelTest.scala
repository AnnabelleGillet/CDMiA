package cdmia.datawrapper.model.hierarchical

import cdmia.core.categorytheory.Category
import org.scalatest.funsuite.*

class JSONModelTest extends AnyFunSuite {
  test("The category of the JSON model can be built") {
    assertResult(true)(JSONModel.category.isInstanceOf[Category])
  }
}
