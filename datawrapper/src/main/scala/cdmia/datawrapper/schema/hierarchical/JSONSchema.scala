package cdmia.datawrapper.schema.hierarchical

import cdmia.datawrapper.modeltransformation.Preservation
import cdmia.datawrapper.model.hierarchical.JSONModel
import cdmia.datawrapper.model.hierarchical.JSONModel._
import cdmia.datawrapper.schema.Schema
import cdmia.core.categorytheory.Category
import cdmia.core.categorytheory.functor.Functor

class JSONSchema(name: String, 
                 val category: Category, 
                 functorTowardsModel: Functor, 
                 preservationVerifications: List[Preservation],
                 val documents: List[Document],
                 val dataTypes: List[DataType],
                 val attributes: List[Attribute],
                 val arrays: List[ArrayAttribute],
                 val arrayIndexes: List[ArrayContent])
  extends Schema(name, JSONModel, functorTowardsModel, preservationVerifications) {

}
