import cdmia.datawrapper.datasource.PropertyGraphNeo4jSource
import cdmia.datawrapper.model.graph.PropertyGraphModel
import cdmia.datawrapper.model.hierarchical.JSONModel
import cdmia.datawrapper.model.relational.RelationalModel
import cdmia.datawrapper.modeltransformation.{SchemaModelTransformation, TemplateTransformation}
import cdmia.core.categorytheory.functor.Functor

import java.nio.file.{FileSystems, Files}

import org.neo4j.driver.AuthTokens
import org.neo4j.driver.GraphDatabase

import scala.collection.JavaConverters._
import sys.process._
	
@main def main(args: String*) =
	val DATA_DIRECTORY: String = "Neo4j/data" // Replace by your own value
	val NEO4J_DIRECTORY: String = "Neo4j/neo4j-community-5.25.1" // Replace by your own value
	val NEO4J_URL: String = "neo4j://localhost" // Replace by your own value
	val NEO4J_USER: String = "" // Replace by your own value
	val NEO4J_PASSWORD: String = "" // Replace by your own value
	// Execute with "scala -classpath neo4j-jdbc-driver-5.25.0.jar:CDMiA-DataWrapper-0.2.0.jar benchmark_execution_neo4j.scala"
	
	val file = java.io.PrintWriter("execution_time_neo4j.csv")

	// Conditions of experiments
	val numberOfRepetitions: Int = 10
	
	// Get schemas
	val directory = FileSystems.getDefault.getPath(DATA_DIRECTORY)
	val schemas = Files
		.list(directory)
		.iterator().asScala
		.map(_.getFileName.toString())
		.filter(_.startsWith("nodes"))		
	
	val template = propertyGraphToRelational()
	file.write(s"schema," + (for (i <- 0 until numberOfRepetitions) yield {
		s"execution_${i}_schema_loading_ms,execution_${i}_migration_application_ms"
	}).mkString(",") + "\n")
	file.flush()
	for (schema <- schemas) {
		println(schema)
		s"""${NEO4J_DIRECTORY}/bin/neo4j stop""".! // Stop Neo4j
		s"""rm -Rf ${NEO4J_DIRECTORY}/data/databases/* ${NEO4J_DIRECTORY}/data/transactions/*""".! // Remove current database
		if (schema.startsWith("nodescreation")) {
			s"""${NEO4J_DIRECTORY}/bin/neo4j-admin database import full --nodes=${DATA_DIRECTORY}/$schema --relationships=${DATA_DIRECTORY}/${schema.replace("nodescreation", "edgescreation")} --overwrite-destination""".! // Execute import for nodes and relationships
		} else {
			s"""${NEO4J_DIRECTORY}/bin/neo4j-admin database import full --nodes=${DATA_DIRECTORY}/$schema --overwrite-destination""".! // Execute import for nodes
		}
		s"""${NEO4J_DIRECTORY}/bin/neo4j start""".! // Start Neo4j
		Thread.sleep(5000)
		
		val neo4jDriver = GraphDatabase.driver(NEO4J_URL, AuthTokens.basic(NEO4J_USER, NEO4J_PASSWORD))
		neo4jDriver.verifyConnectivity()
		
		val neo4jSource = new PropertyGraphNeo4jSource(neo4jDriver)
		val line = s"$schema," + (for (i <- 0 until numberOfRepetitions) yield {
			val result = bench(schema, neo4jSource, template)
			s"${result.schemaLoadingMs},${result.migrationApplicationMs}"
		}).mkString(",")
		println(line)
		file.write(line + "\n")
		file.flush()
	}

	file.close()

case class ExecutionTime(schemaLoadingMs: Long, migrationApplicationMs: Long)

def bench(schema: String, neo4jSource: PropertyGraphNeo4jSource, transformation: TemplateTransformation): ExecutionTime = {
	val schemaLoadingBeginTime = System.currentTimeMillis()
	val categoricalSchema = neo4jSource.getPropertyGraphSchema(schema)
	val schemaLoadingTimeMs = System.currentTimeMillis() - schemaLoadingBeginTime
	
	val migrationApplicationBeginTime = System.currentTimeMillis()
	val result = new SchemaModelTransformation(categoricalSchema, transformation)
	val migrationApplicationTimeMs = System.currentTimeMillis() - migrationApplicationBeginTime
	
	ExecutionTime(schemaLoadingTimeMs, migrationApplicationTimeMs)
}	
	
def propertyGraphToRelational(): TemplateTransformation = {
	new TemplateTransformation("PropertyGraph_to_Relational", PropertyGraphModel, RelationalModel,
		new Functor("PropertyGraph_to_Relational", PropertyGraphModel.category, RelationalModel.category,
		List(
			PropertyGraphModel.vertex ~> RelationalModel.table,
			PropertyGraphModel.edge ~> RelationalModel.foreignKey,
			PropertyGraphModel.label ~> RelationalModel.table,
			PropertyGraphModel.typedAttribute ~> RelationalModel.attributeDomain,
			PropertyGraphModel.boolean ~> RelationalModel.boolean,
			PropertyGraphModel.number ~> RelationalModel.number,
			PropertyGraphModel.string ~> RelationalModel.string,
			PropertyGraphModel.date ~> RelationalModel.date
		),
		List(
			PropertyGraphModel.vertexAttribute ~> RelationalModel.attribute,
			PropertyGraphModel.edgeAttribute ~> (RelationalModel.primaryKeyAttribute o RelationalModel.to),
			PropertyGraphModel.vertexLabel ~> RelationalModel.table.identityMorphism,
			PropertyGraphModel.edgeLabel ~> (RelationalModel.inversePrimaryKey o RelationalModel.to),
			PropertyGraphModel.out ~> (RelationalModel.inversePrimaryKey o RelationalModel.to),
			PropertyGraphModel.in ~> RelationalModel.from,
			PropertyGraphModel.booleanType ~> RelationalModel.booleanAttribute,
			PropertyGraphModel.numberType ~> RelationalModel.numberAttribute,
			PropertyGraphModel.stringType ~> RelationalModel.stringAttribute,
			PropertyGraphModel.dateType ~> RelationalModel.dateAttribute
		)
	))
}
