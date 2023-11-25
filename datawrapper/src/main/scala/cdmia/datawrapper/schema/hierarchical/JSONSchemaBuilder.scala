package cdmia.datawrapper.schema.hierarchical

import cdmia.datawrapper.model.Model
import cdmia.datawrapper.model.hierarchical.JSONModel
import cdmia.datawrapper.model.hierarchical.JSONModel.{ArrayAttribute, ArrayContent, ArrayContentDataType, Attribute, DataType, Document, NestedDocument}
import cdmia.datawrapper.schema.SchemaBuilder
import cdmia.core.categorytheory.Category
import cdmia.core.categorytheory.functor.Functor
import cdmia.datawrapper.model.Model.Element
import cdmia.datawrapper.model.hierarchical.JSONModel.{ArrayAttribute, ArrayContent, Attribute, DataType, Document}

/**
 * Builder used to produce a JSON schema.
 *
 * @param name         : the name of the schema.
 * @param documents    : the [[Document]]s of the schema.
 * @param dataTypes    : the [[DataType]]s of the schema.
 * @param attributes   : the [[Attribute]]s of the schema.
 * @param arrays       : the [[ArrayAttribute]]s of the schema.
 * @param arrayIndexes : the [[ArrayContent]]es of the schema.
 */
class JSONSchemaBuilder(name: String,
                        val documents: List[Document],
                        val dataTypes: List[DataType],
                        val attributes: List[Attribute],
                        val arrays: List[ArrayAttribute],
                        val arrayIndexes: List[ArrayContent]) extends SchemaBuilder(name) {
  override lazy val elements: List[Element] = documents ::: dataTypes ::: attributes ::: arrays ::: arrayIndexes

  /**
   * Add a [[Document]] and return a builder.
   */
  def addDocument(document: Document): JSONSchemaBuilder = {
    document match
      case nested: NestedDocument => require(documents.contains(nested.insideDocument), s"The document ${nested.insideDocument.name} must have been added to the builder.")
      case _ => ()

    new JSONSchemaBuilder(name, documents :+ document, dataTypes, attributes, arrays, arrayIndexes)
  }

  /**
   * Add an [[Attribute]] and return a builder.
   */
  def addAttribute(attribute: Attribute): JSONSchemaBuilder = {
    require(documents.contains(attribute.document), s"The document ${attribute.document.name} must have been added to the builder.")

    val newDataTypes = if (dataTypes.contains(attribute.dataType)) dataTypes else dataTypes :+ attribute.dataType
    new JSONSchemaBuilder(name, documents, newDataTypes, attributes :+ attribute, arrays, arrayIndexes)
  }

  /**
   * Add an [[ArrayAttribute]] and return a builder.
   */
  def addArray(array: ArrayAttribute): JSONSchemaBuilder = {
    require(documents.contains(array.document), s"The document ${array.document.name} must have been added to the builder.")

    new JSONSchemaBuilder(name, documents, dataTypes, attributes, arrays :+ array, arrayIndexes)
  }

  /**
   * Add an [[ArrayContent]] and return a builder.
   */
  def addArrayIndex(arrayIndex: ArrayContent): JSONSchemaBuilder = {
    require(arrays.contains(arrayIndex.array), s"The array ${arrayIndex.array.name} must have been added to the builder.")

    val newDataTypes = arrayIndex match
      case arrayIndexDT: ArrayContentDataType => if (dataTypes.contains(arrayIndexDT.dataType)) {
        dataTypes
      } else {
        dataTypes :+ arrayIndexDT.dataType
      }
      case _ => dataTypes

    new JSONSchemaBuilder(name, documents, newDataTypes, attributes, arrays, arrayIndexes :+ arrayIndex)
  }

  /**
   * Build the schema corresponding to the builder.
   *
   * @return the corresponding [[JSONSchema]].
   */
  override def build(): JSONSchema = {
    val category: Category = buildCategory()
    val functor: Functor = buildFunctor(category, JSONModel)

    new JSONSchema(name, category, functor, elements.flatMap(_.preservationVerifications), documents, dataTypes, attributes, arrays, arrayIndexes)
  }
}

object JSONSchemaBuilder {
  def apply(name: String): JSONSchemaBuilder = {
    new JSONSchemaBuilder(name, List[Document](), List[DataType](), List[Attribute](), List[ArrayAttribute](), List[ArrayContent]())
  }
}
