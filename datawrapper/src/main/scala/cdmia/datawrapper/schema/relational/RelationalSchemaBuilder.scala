package cdmia.datawrapper.schema.relational

import cdmia.datawrapper.Config
import cdmia.datawrapper.model.Model
import cdmia.datawrapper.model.relational.RelationalModel
import cdmia.datawrapper.schema.{Schema, SchemaBuilder}
import cdmia.datawrapper.model.relational.RelationalModel.{Attribute, DataType, ForeignKey, PrimaryKey, RelationalModelElement, Table, foreignKey}
import cdmia.core.categorytheory.{Category, Object, ObjectTransformation}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.{Isomorphism, Morphism, MorphismEquality, MorphismTransformation}
import cdmia.core.categorytheory.pattern.colimit.Colimit
import cdmia.core.categorytheory.pattern.limit.Limit

/**
 * Builder used to produce a relational schema.
 *
 * @param name                 the name of the schema.
 * @param tables               the [[Table]]s of the schema.
 * @param dataTypes            the [[DataType]]s of the schema.
 * @param attributes           the [[Attribute]]s of the schema.
 * @param primaryKeys          the [[PrimaryKey]]s of the schema.
 * @param foreignKeys          the [[ForeignKey]]s of the schema.
 */
private class RelationalSchemaBuilder(
                             name: String,
                             val tables: List[Table],
                             val dataTypes: List[DataType],
                             val attributes: List[Attribute],
                             val primaryKeys: List[PrimaryKey],
                             val foreignKeys: List[ForeignKey]
                             ) extends SchemaBuilder(name) {

  override lazy val elements: List[RelationalModel.RelationalModelElement] = tables ::: dataTypes ::: finalAttributes ::: primaryKeys ::: foreignKeys
  private var finalAttributes: List[Attribute] = attributes

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
    if (!Config.disableRequire) {
      require(tables.contains(attribute.table), s"The table ${attribute.table.name} must have been added to the builder.")
    }
    
    val newDataTypes = if (dataTypes.contains(attribute.dataType)) dataTypes else dataTypes :+ attribute.dataType
    new RelationalSchemaBuilder(name, tables, newDataTypes, attributes :+ attribute, primaryKeys, foreignKeys)
  }

  /**
   * Add a [[PrimaryKey]] and return a builder.
   */
  def addPrimaryKey(primaryKey: PrimaryKey): RelationalSchemaBuilder = {
    if (!Config.disableRequire) {
      require(tables.contains(primaryKey.table), s"The table ${primaryKey.table.name} must have been added to the builder.")
      require(!primaryKeys.map(_.name).contains(primaryKey.name), s"A primary key with the name ${primaryKey.name} has already been added.")
      require(primaryKey.attributes.forall(pka => attributes.contains(pka)),
        s"The attributes ${primaryKey.attributes.filterNot(pka => attributes.contains(pka)).mkString(", ")} must have been added to the builder.")
    }

    new RelationalSchemaBuilder(name, tables, dataTypes, attributes, primaryKeys :+ primaryKey, foreignKeys)
  }

  /**
   * Add a [[ForeignKey]] and return a builder.
   */
  def addForeignKey(foreignKey: ForeignKey): RelationalSchemaBuilder = {
    if (!Config.disableRequire) {
      require(tables.contains(foreignKey.table), s"The table ${foreignKey.table.name} must have been added to the builder.")
      require(primaryKeys.contains(foreignKey.distantPrimaryKey), s"The primary key ${foreignKey.distantPrimaryKey.name} of the table ${foreignKey.distantPrimaryKey.table.name} must have been added to the builder.")
      require(foreignKey.mapping.keys.forall(fka => attributes.contains(fka)),
        s"The attributes ${foreignKey.mapping.keys.filterNot(fka => attributes.contains(fka)).mkString(", ")} must have been added to the builder.")
    }

    new RelationalSchemaBuilder(name, tables, dataTypes, attributes, primaryKeys, foreignKeys :+ foreignKey)
  }

  /**
   * Build the schema corresponding to the builder.
   *
   * @return the corresponding [[RelationalSchema]].
   */
  override def build(): RelationalSchema = {
    // Update object domain of attributes
    val morphismDomainOfAttributes: scala.collection.mutable.Map[Attribute, Morphism] = (for (attribute <- attributes) yield attribute -> attribute.dataTypeMorphism).to(scala.collection.mutable.Map)
    val attributeReferencedBy: scala.collection.mutable.Map[Attribute, List[Attribute]] = (for (attribute <- attributes) yield attribute -> List[Attribute]()).to(scala.collection.mutable.Map)
    val nbOfReferencesByAttribute: scala.collection.mutable.Map[Attribute, Int] = (for (attribute <- attributes) yield attribute -> 0).to(scala.collection.mutable.Map)
    for (foreignKey <- foreignKeys) {
      for ((localAttribute, distantAttribute) <- foreignKey.mapping) {
        attributeReferencedBy(distantAttribute) :+= localAttribute
        nbOfReferencesByAttribute(localAttribute) += 1
      }
    }
    for (attribute <- nbOfReferencesByAttribute.toList.sortWith((e1, e2) => e1._2 < e2._2).map(_._1)) {
      val referencedBy = attributeReferencedBy(attribute)
      for (referencingAttribute <- referencedBy) {
        morphismDomainOfAttributes(referencingAttribute) = morphismDomainOfAttributes(attribute)
      }
    }
    val fromTableToAttributeMorphisms: Map[Attribute, Morphism] = (for (attribute <- attributes) yield attribute -> attribute.produceAttributeMorphism(morphismDomainOfAttributes(attribute).domain)).toMap
    val fromPrimaryKeyDomToAttributeMorphisms: Map[Attribute, Map[PrimaryKey, Morphism]] = (for (attribute <- attributes) yield
      attribute -> attribute.producePrimaryKeyAttributes(
        morphismDomainOfAttributes(attribute).domain,
        primaryKeys.filter(pk => pk.attributes.contains(attribute))
      )).toMap
    finalAttributes = for (attribute <- attributes) yield {
      val inPrimaryKeys = primaryKeys.filter(pk => pk.attributes.contains(attribute))
      val inForeignKeys = foreignKeys.filter(fk => fk.mapping.keys.toList.contains(attribute))
      attribute.produceFinalAttribute(inPrimaryKeys, inForeignKeys, fromTableToAttributeMorphisms, fromPrimaryKeyDomToAttributeMorphisms)
    }

    val category: Category = buildCategory()
    val functor: Functor = buildFunctor(category, RelationalModel)

    new RelationalSchema(name, category, functor, elements.flatMap(_.preservationVerifications), tables, dataTypes, attributes, primaryKeys, foreignKeys)
  }
}

object RelationalSchemaBuilder {
  def apply(name: String): RelationalSchemaBuilder = {
    new RelationalSchemaBuilder(name, List[Table](), List[DataType](), List[Attribute](), List[PrimaryKey](), List[ForeignKey]())
  }
}
