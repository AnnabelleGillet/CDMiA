package cdmia.datawrapper.model.hierarchical

import cdmia.datawrapper.model.Model
import cdmia.core.categorytheory.{Category, CategoryBuilder, Object, ObjectTransformation}
import cdmia.core.categorytheory.morphism.{Morphism, MorphismEquality, MorphismTransformation}

/**
 * Categorical representation of the JSON model.
 */
object JSONModel extends Model("JSON Model") {
  /*
  Objects
   */
  val document: Object = new Object("document")
  val array: Object = new Object("array")
  val typedAttribute: Object = new Object("typed_attribute")
  val number: Object = new Object("number")
  val string: Object = new Object("string")
  val boolean: Object = new Object("boolean")

  override val objects: Iterable[Object] = List[Object](
    document, array, typedAttribute,
    number, string, boolean
  )

  /*
  Morphisms
   */
  val jsonObject: Morphism = new Morphism("object", document, document)
  val arrayAttribute: Morphism = new Morphism("array_attribute", document, array)
  val arrayContent: Morphism = new Morphism("array_content", array, array)
  val documentContent: Morphism = new Morphism("document_content", array, document)
  val attributeContent: Morphism = new Morphism("attribute_content", array, typedAttribute)
  val attribute: Morphism = new Morphism("attribute", document, typedAttribute)
  val numberType: Morphism = new Morphism("number_type", typedAttribute, number)
  val stringType: Morphism = new Morphism("string_type", typedAttribute, string)
  val booleanType: Morphism = new Morphism("boolean_type", typedAttribute, boolean)

  override val morphisms: Iterable[Morphism] = List[Morphism](
    jsonObject, arrayAttribute, arrayContent, documentContent, attributeContent, attribute,
    numberType, stringType, booleanType
  )

  /*
  Constraints
   */

  /*
  Preservations
   */

  /*
  Creations
   */

  /*
  Category
   */
  override val category: Category = CategoryBuilder(this.name, objects, morphisms).build()

  /*
  Elements
   */
  sealed trait JSONModelElement(val name: String) extends Model.Element

  /**
   * Data types
   */
  abstract class DataType(name: String) extends JSONModelElement(name) {
    val obj: Object = new Object(name)

    override val objects: List[Object] = List[Object](obj)
  }

  object NumberType extends DataType("number") {
    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](obj ~> JSONModel.number)
  }

  object StringType extends DataType("string") {
    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](obj ~> JSONModel.string)
  }

  object BooleanType extends DataType("boolean") {
    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](obj ~> JSONModel.boolean)
  }

  private def getAttributeMorphismOfModel(dataType: DataType): Morphism = {
    dataType match
      case NumberType => JSONModel.numberType
      case StringType => JSONModel.stringType
      case BooleanType => JSONModel.booleanType
  }

  /**
   * Document
   *
   * @param name
   */
  class Document(name: String) extends JSONModelElement(name) {
    val obj = new Object(name)

    override val objects: List[Object] = List[Object](obj)

    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](obj ~> JSONModel.document)
  }

  /**
   * Nested document
   *
   * @param name
   * @param insideDocument
   */
  class NestedDocument(val insideDocument: Document, name: String, val nestedDocument: Document) extends Document(name) {
    val objectMorphism: Morphism = new Morphism(s"${name}", insideDocument.obj, nestedDocument.obj)

    override val objects: List[Object] = List[Object]()
    override val morphisms: List[Morphism] = List[Morphism](objectMorphism)

    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation]()
    override val morphismTransformations: List[MorphismTransformation] = List[MorphismTransformation](objectMorphism ~> JSONModel.jsonObject)
  }

  /**
   * General attribute
   */
  class Attribute(name: String, val document: Document, val dataType: DataType) extends JSONModelElement(name) {
    val attributeMorphism: Morphism = new Morphism(name, document.obj, dataType.obj)
    val modelAttributeMorphism: Morphism = getAttributeMorphismOfModel(dataType) o JSONModel.attribute

    override val morphisms: List[Morphism] = List[Morphism](attributeMorphism)

    override val morphismTransformations: List[MorphismTransformation] = List[MorphismTransformation](attributeMorphism ~> modelAttributeMorphism)
  }

  /**
   * Array attribute
   */
  class ArrayAttribute(name: String, val document: Document) extends JSONModelElement(name) {
    val obj: Object = new Object(name)
    val morphismFromDocument: Morphism = new Morphism(s"${name}_array", document.obj, obj)

    override val objects: List[Object] = List[Object](obj)
    override val morphisms: List[Morphism] = List[Morphism](morphismFromDocument)

    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](obj ~> JSONModel.array)
    override val morphismTransformations: List[MorphismTransformation] = List[MorphismTransformation](morphismFromDocument ~> JSONModel.arrayAttribute)
  }

  abstract class ArrayContent(val array: ArrayAttribute) extends JSONModelElement(s"${array.name}_content") {
    val morphism: Morphism
  }

  class DocumentArrayContent(array: ArrayAttribute, val document: Document) extends ArrayContent(array) {
    override val morphism: Morphism = new Morphism(s"${array.name}_contains_document_${document.name}", array.obj, document.obj)

    override val morphisms: List[Morphism] = List[Morphism](morphism)

    override val morphismTransformations: List[MorphismTransformation] = List[MorphismTransformation](morphism ~> JSONModel.documentContent)
  }

  class AttributeArrayContent(array: ArrayAttribute, val dataType: DataType) extends ArrayContent(array) {
    override val morphism: Morphism = new Morphism(s"${array.name}_contains_data_type_${dataType.name}", array.obj, dataType.obj)

    override val morphisms: List[Morphism] = List[Morphism](morphism)

    override val morphismTransformations: List[MorphismTransformation] = List[MorphismTransformation](morphism ~> (getAttributeMorphismOfModel(dataType) o JSONModel.attributeContent))
  }

  class ArrayArrayContent(array: ArrayAttribute, arrayContent: ArrayAttribute) extends ArrayContent(array) {
    override val morphism: Morphism = new Morphism(s"${array.name}_contains_array_${arrayContent.name}", array.obj, arrayContent.obj)

    override val morphisms: List[Morphism] = List[Morphism](morphism)

    override val morphismTransformations: List[MorphismTransformation] = List[MorphismTransformation](morphism ~> JSONModel.arrayContent)
  }
}
