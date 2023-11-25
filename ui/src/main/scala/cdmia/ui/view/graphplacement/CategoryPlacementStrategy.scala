package cdmia.ui.view.graphplacement

import com.brunomnsilva.smartgraph.graph.{Digraph, Edge, Graph, Vertex}
import com.brunomnsilva.smartgraph.graphview.{SmartGraphVertex, SmartPlacementStrategy}
import edu.uci.ics.jung.algorithms.layout.{FRLayout, DAGLayout}
import edu.uci.ics.jung.graph.DirectedSparseGraph
import edu.uci.ics.jung.visualization.{DefaultVisualizationModel, VisualizationModel}

import scala.jdk.CollectionConverters.*
import java.util

/**
 * Strategy that uses a layout from Jung to place the nodes.
 */
class CategoryPlacementStrategy extends SmartPlacementStrategy {
  override def place[V, E](width: Double, height: Double, graph: Graph[V, E], vertices: util.Collection[_ <: SmartGraphVertex[V]]): Unit = {
    require(graph.isInstanceOf[Digraph[_, _]], "The graph must be directed.")
    val dag = new DirectedSparseGraph[V, E]()
    vertices.forEach(vertex => {
      dag.addVertex(vertex.getUnderlyingVertex.element())
    })
    graph.edges().forEach(edge => {
      dag.addEdge(edge.element(), edge.vertices()(0).element(), edge.vertices()(1).element())
    })
    var dagLayoutApplied = false
    val layout = new FRLayout[V, E](dag)
    layout.setSize(new java.awt.Dimension(width.toInt - 20, height.toInt - 20))
    new DefaultVisualizationModel[V, E](layout, new java.awt.Dimension(width.toInt - 20, height.toInt - 20))
    vertices.forEach(vertex => {
      // Get the position of the vertex
      val positionedVertex = dag.getVertices.asScala.filter(_ == vertex.getUnderlyingVertex.element()).head
      // Set the position
      vertex.setPosition(layout.getX(positionedVertex) + 10, layout.getY(positionedVertex) + 10)
    })
  }
}
