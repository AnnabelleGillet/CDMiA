package cdmia.datawrapper.model.graph

import cdmia.core.categorytheory.Category
import org.scalatest.funsuite.*

class PropertyGraphModelTest extends AnyFunSuite {
  test("The category of the property graph model can be built") {
    assertResult(true)(PropertyGraphModel.category.isInstanceOf[Category])
  }
}