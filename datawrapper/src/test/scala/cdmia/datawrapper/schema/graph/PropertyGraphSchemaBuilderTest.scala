package cdmia.datawrapper.schema.graph

import cdmia.datawrapper.model.graph.PropertyGraphModel._
import org.scalatest.funsuite.*

class PropertyGraphSchemaBuilderTest extends AnyFunSuite {
  test("An empty schema can be built") {
    assertResult(true)(PropertyGraphSchemaBuilder("test").build().isInstanceOf[PropertyGraphSchema])
  }

  test("A schema with a vertex can be built") {
    val vertex = new Vertex("v")
    val builder = PropertyGraphSchemaBuilder("test").addVertex(vertex)
    assertResult(true)(builder.build().isInstanceOf[PropertyGraphSchema])
  }

  test("A schema with a vertex and an attribute can be built") {
    val vertex = new Vertex("v")
    val builder = PropertyGraphSchemaBuilder("test").addVertex(vertex)
      .addVertexAttribute(new VertexAttribute("va", vertex, StringType))
    assertResult(true)(builder.build().isInstanceOf[PropertyGraphSchema])
  }

  test("Adding an attribute without the corresponding vertex throws an exception") {
    val vertex = new Vertex("v")
    val builder = PropertyGraphSchemaBuilder("test").addVertex(vertex)
    assertThrows[IllegalArgumentException](builder.addVertexAttribute(new VertexAttribute("va", new Vertex("v2"), StringType)))
  }

  test("A schema with a vertex and a label can be built") {
    val vertex = new Vertex("v")
    val builder = PropertyGraphSchemaBuilder("test").addVertex(vertex)
      .addVertexLabel(new VertexLabel(vertex, new Label("l")))
    assertResult(true)(builder.build().isInstanceOf[PropertyGraphSchema])
  }

  test("Adding a label without the corresponding vertex throws an exception") {
    val vertex = new Vertex("v")
    val builder = PropertyGraphSchemaBuilder("test").addVertex(vertex)
    assertThrows[IllegalArgumentException](builder.addVertexLabel(new VertexLabel(new Vertex("v2"), new Label("l"))))
  }

  test("A schema with a vertex and an edge can be built") {
    val vertex = new Vertex("v")
    val builder = PropertyGraphSchemaBuilder("test")
      .addVertex(vertex)
      .addEdge(new Edge("e", vertex, vertex))
    assertResult(true)(builder.build().isInstanceOf[PropertyGraphSchema])
  }

  test("A schema with two vertices and an edge can be built") {
    val in = new Vertex("vin")
    val out = new Vertex("vout")
    val builder = PropertyGraphSchemaBuilder("test")
      .addVertex(in)
      .addVertex(out)
      .addEdge(new Edge("e", in, out))
    assertResult(true)(builder.build().isInstanceOf[PropertyGraphSchema])
  }

  test("Adding an edge without the corresponding vertex throws an exception") {
    val vertex = new Vertex("v")
    val builder = PropertyGraphSchemaBuilder("test")
      .addVertex(vertex)
    assertThrows[IllegalArgumentException](builder.addEdge(new Edge("e", new Vertex("v2"), vertex)))
    assertThrows[IllegalArgumentException](builder.addEdge(new Edge("e", vertex, new Vertex("v2"))))
    assertThrows[IllegalArgumentException](builder.addEdge(new Edge("e", new Vertex("v2"), new Vertex("v3"))))
  }

  test("A schema with an edge and an attribute can be built") {
    val in = new Vertex("vin")
    val out = new Vertex("vout")
    val edge = new Edge("e", in, out)
    val builder = PropertyGraphSchemaBuilder("test")
      .addVertex(in)
      .addVertex(out)
      .addEdge(edge)
      .addEdgeAttribute(new EdgeAttribute("va", edge, StringType))
    assertResult(true)(builder.build().isInstanceOf[PropertyGraphSchema])
  }

  test("Adding an attribute without the corresponding edge throws an exception") {
    val vertex = new Vertex("v")
    val builder = PropertyGraphSchemaBuilder("test").addVertex(vertex)
    assertThrows[IllegalArgumentException](builder.addEdgeAttribute(new EdgeAttribute("ea", new Edge("v2", vertex, vertex), StringType)))
  }

  test("A schema with an edge and a label can be built") {
    val in = new Vertex("vin")
    val out = new Vertex("vout")
    val edge = new Edge("e", in, out)
    val builder = PropertyGraphSchemaBuilder("test")
      .addVertex(in)
      .addVertex(out)
      .addEdge(edge)
      .addEdgeLabel(new EdgeLabel(edge, new Label("l")))
    assertResult(true)(builder.build().isInstanceOf[PropertyGraphSchema])
  }

  test("Adding a label without the corresponding edge throws an exception") {
    val vertex = new Vertex("v")
    val builder = PropertyGraphSchemaBuilder("test").addVertex(vertex)
    assertThrows[IllegalArgumentException](builder.addEdgeLabel(new EdgeLabel(new Edge("v2", vertex, vertex), new Label("l"))))
  }
}
