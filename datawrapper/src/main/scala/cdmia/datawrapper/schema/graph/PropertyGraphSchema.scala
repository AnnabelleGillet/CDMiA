package cdmia.datawrapper.schema.graph

import cdmia.datawrapper.model.graph.PropertyGraphModel
import cdmia.datawrapper.model.graph.PropertyGraphModel._
import cdmia.datawrapper.modeltransformation.Preservation
import cdmia.datawrapper.schema.Schema
import cdmia.core.categorytheory.Category
import cdmia.core.categorytheory.functor.Functor

class PropertyGraphSchema(name: String,
                          val category: Category,
                          functorTowardsModel: Functor,
                          preservationVerifications: List[Preservation],
                          val vertices: List[Vertex],
                          val edges: List[Edge],
                          val dataTypes: List[DataType],
                          val labels: List[Label],
                          val vertexAttributes: List[VertexAttribute],
                          val edgeAttributes: List[EdgeAttribute],
                          val vertexLabels: List[VertexLabel],
                          val edgeLabels: List[EdgeLabel])
  extends Schema(name, PropertyGraphModel, functorTowardsModel, preservationVerifications) {

}
