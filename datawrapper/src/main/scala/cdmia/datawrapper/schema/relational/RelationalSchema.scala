package cdmia.datawrapper.schema.relational

import cdmia.datawrapper.model.relational.RelationalModel
import cdmia.datawrapper.model.relational.RelationalModel.*
import cdmia.datawrapper.modeltransformation.Preservation
import cdmia.datawrapper.schema.Schema
import cdmia.core.categorytheory.{Category, Object, ObjectTransformation}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.{Isomorphism, Morphism, MorphismEquality, MorphismTransformation}
import cdmia.core.categorytheory.pattern.colimit.Colimit
import cdmia.core.categorytheory.pattern.limit.{Limit, Pullback}
import cdmia.datawrapper.model.relational.RelationalModel
import cdmia.datawrapper.model.relational.RelationalModel.{Attribute, DataType, ForeignKey, PrimaryKey, Table}
import cdmia.datawrapper.modeltransformation.Preservation

class RelationalSchema(name: String,
                       val category: Category,
                       functorTowardsModel: Functor,
                       preservationVerifications: List[Preservation],
                       val tables: List[Table],
                       val dataTypes: List[DataType],
                       val attributes: List[Attribute],
                       val primaryKeys: List[PrimaryKey],
                       val foreignKeys: List[ForeignKey])
  extends Schema(name, RelationalModel, functorTowardsModel, preservationVerifications) {

}

