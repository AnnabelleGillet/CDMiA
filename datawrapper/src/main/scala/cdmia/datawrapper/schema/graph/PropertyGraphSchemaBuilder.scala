package cdmia.datawrapper.schema.graph

import cdmia.datawrapper.model.Model
import cdmia.datawrapper.model.graph.PropertyGraphModel
import cdmia.datawrapper.model.graph.PropertyGraphModel.{DataType, Edge, EdgeAttribute, EdgeLabel, Label, Vertex, VertexAttribute, VertexLabel}
import cdmia.datawrapper.schema.{Schema, SchemaBuilder}
import cdmia.core.categorytheory.Category
import cdmia.core.categorytheory.functor.Functor
import cdmia.datawrapper.model.graph.PropertyGraphModel.{DataType, Edge, EdgeAttribute, EdgeLabel, Label, Vertex, VertexAttribute, VertexLabel}

class PropertyGraphSchemaBuilder(name: String,
                                 val vertices: List[Vertex],
                                 val edges: List[Edge],
                                 val dataTypes: List[DataType],
                                 val labels: List[Label],
                                 val vertexAttributes: List[VertexAttribute],
                                 val edgeAttributes: List[EdgeAttribute],
                                 val vertexLabels: List[VertexLabel],
                                 val edgeLabels: List[EdgeLabel]
                                ) extends SchemaBuilder(name) {

  override lazy val elements: List[PropertyGraphModel.PropertyGraphModelElement] = vertices ::: edges ::: dataTypes ::: labels :::
    vertexAttributes ::: edgeAttributes ::: vertexLabels ::: edgeLabels

  /**
   * Add a [[Vertex]] and return a builder.
   */
  def addVertex(vertex: Vertex): PropertyGraphSchemaBuilder = {
    new PropertyGraphSchemaBuilder(name, vertices :+ vertex, edges, dataTypes, labels, vertexAttributes, edgeAttributes, vertexLabels, edgeLabels)
  }

  /**
   * Add an [[Edge]] and return a builder.
   */
  def addEdge(edge: Edge): PropertyGraphSchemaBuilder = {
    require(vertices.contains(edge.in), s"The in vertex ${edge.in.name} must have been added to the builder.")
    require(vertices.contains(edge.out), s"The out vertex ${edge.out.name} must have been added to the builder.")

    new PropertyGraphSchemaBuilder(name, vertices, edges :+ edge, dataTypes, labels, vertexAttributes, edgeAttributes, vertexLabels, edgeLabels)
  }

  /**
   * Add a [[VertexAttribute]] and return a builder.
   */
  def addVertexAttribute(attribute: VertexAttribute): PropertyGraphSchemaBuilder = {
    require(vertices.contains(attribute.vertex), s"The vertex ${attribute.vertex.name} must have been added to the builder.")

    val newDataTypes = if (dataTypes.contains(attribute.dataType)) dataTypes else dataTypes :+ attribute.dataType
    new PropertyGraphSchemaBuilder(name, vertices, edges, newDataTypes, labels, vertexAttributes :+ attribute, edgeAttributes, vertexLabels, edgeLabels)
  }

  /**
   * Add an [[EdgeAttribute]] and return a builder.
   */
  def addEdgeAttribute(attribute: EdgeAttribute): PropertyGraphSchemaBuilder = {
    require(edges.contains(attribute.edge), s"The edge ${attribute.edge.name} must have been added to the builder.")

    val newDataTypes = if (dataTypes.contains(attribute.dataType)) dataTypes else dataTypes :+ attribute.dataType
    new PropertyGraphSchemaBuilder(name, vertices, edges, newDataTypes, labels, vertexAttributes, edgeAttributes :+ attribute, vertexLabels, edgeLabels)
  }

  /**
   * Add a [[VertexLabel]] and return a builder.
   */
  def addVertexLabel(label: VertexLabel): PropertyGraphSchemaBuilder = {
    require(vertices.contains(label.vertex), s"The vertex ${label.vertex.name} must have been added to the builder.")

    val newLabels = if (labels.contains(label.label)) labels else labels :+ label.label
    new PropertyGraphSchemaBuilder(name, vertices, edges, dataTypes, newLabels, vertexAttributes, edgeAttributes, vertexLabels :+ label, edgeLabels)
  }

  /**
   * Add an [[EdgeLabel]] and return a builder.
   */
  def addEdgeLabel(label: EdgeLabel): PropertyGraphSchemaBuilder = {
    require(edges.contains(label.edge), s"The edge ${label.edge.name} must have been added to the builder.")

    val newLabels = if (labels.contains(label.label)) labels else labels :+ label.label
    new PropertyGraphSchemaBuilder(name, vertices, edges, dataTypes, newLabels, vertexAttributes, edgeAttributes, vertexLabels, edgeLabels :+ label)
  }

  /**
   * Build the schema corresponding to the builder.
   *
   * @return the corresponding [[PropertyGraphSchema]].
   */
  override def build(): PropertyGraphSchema = {
    val category: Category = buildCategory()
    val functor: Functor = buildFunctor(category, PropertyGraphModel)

    new PropertyGraphSchema(name, category, functor, elements.flatMap(_.preservationVerifications), vertices, edges, dataTypes, labels, vertexAttributes, edgeAttributes, vertexLabels, edgeLabels)
  }
}

object PropertyGraphSchemaBuilder {
  def apply(name: String): PropertyGraphSchemaBuilder = {
    new PropertyGraphSchemaBuilder(name, List[Vertex](), List[Edge](), List[DataType](), List[Label](), List[VertexAttribute](), List[EdgeAttribute](), List[VertexLabel](), List[EdgeLabel]())
  }
}
