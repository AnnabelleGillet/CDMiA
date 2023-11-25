package cdmia.datawrapper.schema

import cdmia.datawrapper.model.Model
import cdmia.datawrapper.modeltransformation.Preservation
import cdmia.core.categorytheory.Category
import cdmia.core.categorytheory.functor.Functor

abstract class Schema(val name: String, val model: Model, 
                      val functorTowardsModel: Functor, val preservationVerifications: List[Preservation]) extends Serializable {
  val category: Category
}
