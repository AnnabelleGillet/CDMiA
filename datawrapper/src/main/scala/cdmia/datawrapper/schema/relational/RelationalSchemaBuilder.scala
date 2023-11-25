package cdmia.datawrapper.schema.relational

import cdmia.datawrapper.model.Model
import cdmia.datawrapper.model.relational.RelationalModel
import cdmia.datawrapper.schema.{Schema, SchemaBuilder}
import cdmia.datawrapper.model.relational.RelationalModel.{Attribute, ComposedForeignKeyAttribute, ComposedPrimaryKeyAttribute, DataType, ForeignKey, ForeignKeyAttribute, PrimaryKey, PrimaryKeyAttribute, RelationalModelElement, Table}
import cdmia.core.categorytheory.{Category, Object, ObjectTransformation}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.{Isomorphism, Morphism, MorphismEquality, MorphismTransformation}
import cdmia.core.categorytheory.pattern.colimit.Colimit
import cdmia.core.categorytheory.pattern.limit.Limit

/**
 * Builder used to produce a relational schema.
 *
 * @param name                 : the name of the schema.
 * @param tables               : the [[Table]]s of the schema.
 * @param dataTypes            : the [[DataType]]s of the schema.
 * @param attributes           : the [[Attribute]]s of the schema.
 * @param primaryKeys          : the [[PrimaryKey]]s of the schema.
 * @param foreignKeys          : the [[ForeignKeyAttribute]]s of the schema.
 */
private class RelationalSchemaBuilder(
                             name: String,
                             val tables: List[Table],
                             val dataTypes: List[DataType],
                             val attributes: List[Attribute],
                             val primaryKeys: List[PrimaryKey],
                             val foreignKeys: List[ForeignKey]
                             ) extends SchemaBuilder(name) {

  override lazy val elements: List[RelationalModel.RelationalModelElement] = tables ::: dataTypes ::: attributes ::: primaryKeys ::: foreignKeys

  /**
   * Add a [[Table]] and return a builder.
   */
  def addTable(table: Table): RelationalSchemaBuilder = {
    new RelationalSchemaBuilder(name, tables :+ table, dataTypes, attributes, primaryKeys, foreignKeys)
  }

  /**
   * Add an [[Attribute]] and return a builder.
   */
  def addAttribute(attribute: Attribute): RelationalSchemaBuilder = {
    require(tables.contains(attribute.table), s"The table ${attribute.table.name} must have been added to the builder.")
    
    val newDataTypes = if (dataTypes.contains(attribute.dataType)) dataTypes else dataTypes :+ attribute.dataType
    new RelationalSchemaBuilder(name, tables, newDataTypes, attributes :+ attribute, primaryKeys, foreignKeys)
  }

  /**
   * Add a [[PrimaryKeyAttribute]] and return a builder.
   */
  def addPrimaryKeyAttribute(primaryKeyAttribute: PrimaryKeyAttribute): RelationalSchemaBuilder = {
    require(tables.contains(primaryKeyAttribute.table), s"The table ${primaryKeyAttribute.table.name} must have been added to the builder.")

    val newDataTypes = if (dataTypes.contains(primaryKeyAttribute.dataType)) dataTypes else dataTypes :+ primaryKeyAttribute.dataType
    new RelationalSchemaBuilder(name, tables, newDataTypes, attributes, primaryKeys :+ primaryKeyAttribute, foreignKeys)
  }

  /**
   * Add a [[ComposedPrimaryKeyAttribute]] and return a builder.
   */
  def addComposedPrimaryKeyAttribute(composedPrimaryKeyAttribute: ComposedPrimaryKeyAttribute): RelationalSchemaBuilder = {
    require(tables.contains(composedPrimaryKeyAttribute.table), s"The table ${composedPrimaryKeyAttribute.table.name} must have been added to the builder.")
    
    var newDataTypes = dataTypes
    for (attribute <- composedPrimaryKeyAttribute.individualAttributes) {
      if (!newDataTypes.contains(attribute.dataType)) {
        newDataTypes:+= attribute.dataType
      }
    }
    new RelationalSchemaBuilder(name, tables, newDataTypes, attributes, primaryKeys :+ composedPrimaryKeyAttribute, foreignKeys)
  }

  /**
   * Add a [[ForeignKeyAttribute]] and return a builder.
   */
  def addForeignKeyAttribute(foreignKeyAttribute: ForeignKeyAttribute): RelationalSchemaBuilder = {
    require(tables.contains(foreignKeyAttribute.table), s"The table ${foreignKeyAttribute.table.name} must have been added to the builder.")
    require(primaryKeys.contains(foreignKeyAttribute.distantPrimaryKey), s"The primary key ${foreignKeyAttribute.distantPrimaryKey.name} of the table ${foreignKeyAttribute.distantPrimaryKey.table.name} must have been added to the builder.")
    new RelationalSchemaBuilder(name, tables, dataTypes, attributes, primaryKeys, foreignKeys :+ foreignKeyAttribute)
  }

  /**
   * Add a [[ComposedPrimaryKeyAttribute]] and return a builder.
   */
  def addComposedForeignKeyAttribute(foreignKeyAttribute: ComposedForeignKeyAttribute): RelationalSchemaBuilder = {
    require(tables.contains(foreignKeyAttribute.table), s"The table ${foreignKeyAttribute.table.name} must have been added to the builder.")
    require(primaryKeys.contains(foreignKeyAttribute.distantPrimaryKey), s"The primary key ${foreignKeyAttribute.distantPrimaryKey.name} of the table ${foreignKeyAttribute.distantPrimaryKey.table.name} must have been added to the builder.")
    new RelationalSchemaBuilder(name, tables, dataTypes, attributes, primaryKeys, foreignKeys :+ foreignKeyAttribute)
  }

  /**
   * Build the schema corresponding to the builder.
   *
   * @return the corresponding [[RelationalSchema]].
   */
  override def build(): RelationalSchema = {
    val category: Category = buildCategory()
    val functor: Functor = buildFunctor(category, RelationalModel)

    new RelationalSchema(name, category, functor, elements.flatMap(_.preservationVerifications), tables, dataTypes, attributes, primaryKeys, foreignKeys)
  }
}

object RelationalSchemaBuilder {
  def apply(name: String): RelationalSchemaBuilder = {
    new RelationalSchemaBuilder(name, List[Table](), List[DataType](), List[Attribute](), List[PrimaryKeyAttribute](), List[ForeignKeyAttribute]())
  }
}
