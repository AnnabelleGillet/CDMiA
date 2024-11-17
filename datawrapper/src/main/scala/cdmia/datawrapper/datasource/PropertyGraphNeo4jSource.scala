package cdmia.datawrapper.datasource

import cdmia.datawrapper.model.graph.PropertyGraphModel
import cdmia.datawrapper.schema.graph.{PropertyGraphSchema, PropertyGraphSchemaBuilder}
import org.neo4j.driver.{Driver, QueryConfig}

class PropertyGraphNeo4jSource(val driver: Driver) {
  cdmia.core.categorytheory.Config.disableRequire = true
  cdmia.datawrapper.Config.disableRequire = true

  private case class Neo4jEdge(name: String,
                               var in: Set[String] = Set[String](),
                               var out: Set[String] = Set[String](),
                               var properties: Map[String, String] = Map[String, String]()
                              )

  /**
   * Build a [[PropertyGraphSchema]] from the given driver.
   *
   * @param schemaName the name of the schema to build.
   * @return
   */
  def getPropertyGraphSchema(schemaName: String): PropertyGraphSchema = {
    var propertyGraphSchemaBuilder = PropertyGraphSchemaBuilder(schemaName)
    var nodes = Map[String, PropertyGraphModel.Vertex]()
    var labelToNodes = Map[String, List[PropertyGraphModel.Vertex]]()
    var constructibleEdges = Map[String, Neo4jEdge]()
    var edges = Map[String, PropertyGraphModel.Edge]()

    // Add vertices
    val nodeResult = driver.executableQuery("CALL db.schema.nodeTypeProperties()")
      .withConfig(QueryConfig.builder().withDatabase("neo4j").build())
      .execute()

    nodeResult.records().forEach(record => {
      val node = record.get("nodeType").asString().replace("`", "").replace("\"", "")
      if (!nodes.contains(node)) {
        val pgNode = PropertyGraphModel.Vertex(node)
        nodes += node -> pgNode
        propertyGraphSchemaBuilder = propertyGraphSchemaBuilder.addVertex(pgNode)
        record.get("nodeLabels").asList().asInstanceOf[java.util.List[String]].forEach(label => {
          propertyGraphSchemaBuilder = propertyGraphSchemaBuilder.addVertexLabel(PropertyGraphModel.VertexLabel(pgNode, label))
          labelToNodes += label -> (labelToNodes.getOrElse(label, List[PropertyGraphModel.Vertex]()) :+ pgNode)
        })
      }
      val pgNode = nodes(node)

      val propertyName = record.get("propertyName").asString
      val propertyType = record.get("propertyTypes").asList[String](_.toString.replace("\"", "")).get(0)

      propertyGraphSchemaBuilder = propertyGraphSchemaBuilder.addVertexAttribute(PropertyGraphModel.VertexAttribute(propertyName, pgNode, neo4jTypeToModelType(propertyType)))
    })

    // Add edges
    val edgeResult = driver.executableQuery("CALL apoc.meta.schema() YIELD value as schemaMap UNWIND keys(schemaMap) as label WITH label, schemaMap[label] as data WHERE data.type = \"node\" RETURN label, data")
      .withConfig(QueryConfig.builder().withDatabase("neo4j").build())
      .execute()

    edgeResult.records().forEach(record => {
      val nodeLabel = record.get("label").asString()

      record.get("data").asMap().get("relationships").asInstanceOf[java.util.Map[String, java.util.Map[String, Any]]].forEach((relationshipLabel, v) => {
        val noe4jEdge = constructibleEdges.getOrElse(relationshipLabel, Neo4jEdge(relationshipLabel))
        if (!constructibleEdges.contains(relationshipLabel)) {
          constructibleEdges += relationshipLabel -> noe4jEdge
        }

        val direction = v.get("direction").asInstanceOf[String]
        val nodeLabels = v.get("labels").asInstanceOf[java.util.List[String]]

        if (direction == "in") {
          noe4jEdge.in += nodeLabel
          nodeLabels.forEach(noe4jEdge.out += _)
        } else {
          noe4jEdge.out += nodeLabel
          nodeLabels.forEach(noe4jEdge.in += _)
        }

        val properties = v.get("properties").asInstanceOf[java.util.Map[String, java.util.Map[String, Any]]]
        properties.forEach((propertyName, v2) => {
          noe4jEdge.properties += propertyName -> v2.get("type").toString
        })
      })
    })

    for ((relationshipName, neo4jEdge) <- constructibleEdges) {
      val inNodes = neo4jEdge.in.flatMap(labelToNodes(_))
      val outNodes = neo4jEdge.out.flatMap(labelToNodes(_))

      for (inNode <- inNodes; outNode <- outNodes) {
        val edge = PropertyGraphModel.Edge(relationshipName, inNode, outNode)
        edges += relationshipName -> edge
        propertyGraphSchemaBuilder = propertyGraphSchemaBuilder.addEdge(edge)
        propertyGraphSchemaBuilder = propertyGraphSchemaBuilder.addEdgeLabel(PropertyGraphModel.EdgeLabel(edge, relationshipName))
        for ((propertyName, propertyType) <- neo4jEdge.properties) {
          propertyGraphSchemaBuilder = propertyGraphSchemaBuilder.addEdgeAttribute(PropertyGraphModel.EdgeAttribute(propertyName, edge, neo4jTypeToModelType(propertyType)))
        }
      }
    }

    propertyGraphSchemaBuilder.build()
  }

  private def neo4jTypeToModelType(noe4jType: String): PropertyGraphModel.DataType = {
    noe4jType.toUpperCase match
      case "STRING" => PropertyGraphModel.StringType
      case "FLOAT" => PropertyGraphModel.NumberType
      case "LONG" => PropertyGraphModel.NumberType
      case "INTEGER" => PropertyGraphModel.NumberType
      case "BOOLEAN" => PropertyGraphModel.BooleanType
      case _ => PropertyGraphModel.StringType
  }
}
