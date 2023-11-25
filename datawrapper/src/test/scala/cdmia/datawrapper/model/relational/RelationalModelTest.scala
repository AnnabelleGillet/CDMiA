package cdmia.datawrapper.model.relational

import cdmia.core.categorytheory.Category
import org.scalatest.funsuite.*

class RelationalModelTest extends AnyFunSuite {
  test("The category of the relational model can be built") {
    assertResult(true)(RelationalModel.category.isInstanceOf[Category])
  }
}
