package cdmia.datawrapper.model.graph

import cdmia.datawrapper.model.Model
import cdmia.datawrapper.modeltransformation.{Creation, PatternCreation, PatternPreservation, Preservation}
import cdmia.core.categorytheory.morphism.{Morphism, MorphismTransformation}
import cdmia.core.categorytheory.{Category, CategoryBuilder, Object, ObjectTransformation}
import cdmia.core.categorytheory.pattern.limit.{Limit, Product}

/**
 * Categorical representation of the property graph model.
 */
object PropertyGraphModel extends Model("Property graph Model") {
  /*
  Objects
   */
  val vertex: Object = new Object("vertex")
  val edge: Object = new Object("edge")
  val typedAttribute: Object = new Object("typed_attribute")
  val label: Object = new Object("label")
  val number: Object = new Object("number")
  val string: Object = new Object("string")
  val boolean: Object = new Object("boolean")
  val date: Object = new Object("date")

  override val objects: Iterable[Object] = List[Object](
    vertex, edge, label,
    typedAttribute, number, string, boolean, date
  )

  /*
  Morphisms
   */
  val in: Morphism = new Morphism("in", edge, vertex)
  val out: Morphism = new Morphism("out", edge, vertex)
  val vertexAttribute: Morphism = new Morphism("vertex_attribute", vertex, typedAttribute)
  val edgeAttribute: Morphism = new Morphism("edge_attribute", edge, typedAttribute)
  val numberType: Morphism = new Morphism("number_type", typedAttribute, number)
  val stringType: Morphism = new Morphism("string_type", typedAttribute, string)
  val booleanType: Morphism = new Morphism("boolean_type", typedAttribute, boolean)
  val dateType: Morphism = new Morphism("date_type", typedAttribute, date)
  val vertexLabel: Morphism = new Morphism("vertex_label", vertex, label)
  val edgeLabel: Morphism = new Morphism("edge_label", edge, label)

  override val morphisms: Iterable[Morphism] = List[Morphism](
    in, out, vertexAttribute, edgeAttribute,
    numberType, stringType, booleanType, dateType,
    vertexLabel, edgeLabel
  )

  /*
  Constraints
   */
  val edgeProduct: Product = new Product(edge, List[Object](edge, vertex), List[Morphism](edge.identityMorphism, in, out), "edge_product")

  /*
  Preservations
   */
  override val preservationVerifications: List[Preservation] = List[Preservation](
    new PatternPreservation("Edge product", edgeProduct)
  )

  /*
  Creations
   */
  override val creationVerifications: List[Creation] = List[Creation](
    new PatternCreation("Edge product", edgeProduct, List[Object](edge), List[Morphism](in, out, edge.identityMorphism))
  )

  /*
  Category
   */
  override val category: Category = CategoryBuilder(this.name, objects, morphisms).build(List[Limit](edgeProduct))

  /*
  Elements
   */
  sealed trait PropertyGraphModelElement(val name: String) extends Model.Element

  /**
   * Vertex
   *
   * @param name
   */
  class Vertex(name: String) extends PropertyGraphModelElement(name) {
    val obj: Object = new Object(name)

    override val objects: List[Object] = List[Object](obj)

    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](obj ~> PropertyGraphModel.vertex)
  }

  /**
   * Edge
   *
   * @param name
   * @param in
   * @param out
   */
  class Edge(name: String, val in: Vertex, val out: Vertex) extends PropertyGraphModelElement(name) {
    val obj: Object = new Object(name)
    val inMorphism: Morphism = new Morphism(s"${name}_in", obj, in.obj)
    val outMorphism: Morphism = new Morphism(s"${name}_out", obj, out.obj)

    val product: Product = new Product(obj, List[Object](obj, in.obj, out.obj).distinct, List[Morphism](obj.identityMorphism, inMorphism, outMorphism), s"${name}_edge_product")

    override val objects: List[Object] = List[Object](obj)
    override val morphisms: List[Morphism] = List[Morphism](inMorphism, outMorphism)

    override val limits: List[Limit] = List[Limit](product)

    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](obj ~> PropertyGraphModel.edge)
    override val morphismTransformations: List[MorphismTransformation] = List[MorphismTransformation](inMorphism ~> PropertyGraphModel.in, outMorphism ~> PropertyGraphModel.out)

    override val preservationVerifications: List[Preservation] = List[Preservation](new PatternPreservation(s"Edge $name product", product))
  }

  /**
   * DataType
   *
   * @param name
   */
  abstract class DataType(name: String) extends PropertyGraphModelElement(name) {
    val obj: Object = new Object(name)

    override val objects: List[Object] = List[Object](obj)
  }

  object NumberType extends DataType("number") {
    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](obj ~> PropertyGraphModel.number)
  }

  object StringType extends DataType("string") {
    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](obj ~> PropertyGraphModel.string)
  }

  object BooleanType extends DataType("boolean") {
    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](obj ~> PropertyGraphModel.boolean)
  }

  object DateType extends DataType("date") {
    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](obj ~> PropertyGraphModel.date)
  }

  private def getAttributeMorphismOfModel(dataType: DataType): Morphism = {
    dataType match
      case NumberType => PropertyGraphModel.numberType
      case StringType => PropertyGraphModel.stringType
      case BooleanType => PropertyGraphModel.booleanType
      case DateType => PropertyGraphModel.dateType
  }

  /**
   * Vertex attribute
   *
   * @param name
   * @param vertex
   * @param dataType
   */
  class VertexAttribute(name: String, val vertex: Vertex, val dataType: DataType) extends PropertyGraphModelElement(name) {
    val morphism: Morphism = new Morphism(name, vertex.obj, dataType.obj)

    val modelTypeMorphism: Morphism = getAttributeMorphismOfModel(dataType)

    override val morphisms: List[Morphism] = List[Morphism](morphism)

    override val morphismTransformations: List[MorphismTransformation] = List[MorphismTransformation](morphism ~> (modelTypeMorphism o PropertyGraphModel.vertexAttribute))
  }

  /**
   * Edge attribute
   *
   * @param name
   * @param edge
   * @param dataType
   */
  class EdgeAttribute(name: String, val edge: Edge, val dataType: DataType) extends PropertyGraphModelElement(name) {
    val morphism: Morphism = new Morphism(name, edge.obj, dataType.obj)

    val modelTypeMorphism: Morphism = getAttributeMorphismOfModel(dataType)

    override val morphisms: List[Morphism] = List[Morphism](morphism)

    override val morphismTransformations: List[MorphismTransformation] = List[MorphismTransformation](morphism ~> (modelTypeMorphism o PropertyGraphModel.edgeAttribute))
  }

  /**
   * Label
   */
  class Label(name: String) extends PropertyGraphModelElement(name) {
    val obj: Object = new Object(name)

    override val objects: List[Object] = List[Object](obj)

    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](obj ~> PropertyGraphModel.label)
  }

  /**
   * Vertex label
   *
   * @param name
   * @param vertex
   */
  class VertexLabel(val vertex: Vertex, val label: Label) extends PropertyGraphModelElement(s"${vertex.name}_label_${label.name}") {
    val morphism: Morphism = new Morphism(name, vertex.obj, label.obj)

    override val morphisms: List[Morphism] = List[Morphism](morphism)

    override val morphismTransformations: List[MorphismTransformation] = List[MorphismTransformation](morphism ~> PropertyGraphModel.vertexLabel)
  }

  /**
   * Edge label
   *
   * @param name
   * @param vertex
   */
  class EdgeLabel(val edge: Edge, val label: Label) extends PropertyGraphModelElement(s"${edge.name}_label_${label.name}") {
    val morphism: Morphism = new Morphism(name, edge.obj, label.obj)

    override val morphisms: List[Morphism] = List[Morphism](morphism)

    override val morphismTransformations: List[MorphismTransformation] = List[MorphismTransformation](morphism ~> PropertyGraphModel.edgeLabel)
  }
}
