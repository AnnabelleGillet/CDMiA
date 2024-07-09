import cdmia.datawrapper.datasource.RelationalJDBCSource
import cdmia.datawrapper.model.graph.PropertyGraphModel
import cdmia.datawrapper.model.hierarchical.JSONModel
import cdmia.datawrapper.model.relational.RelationalModel
import cdmia.datawrapper.modeltransformation.{SchemaModelTransformation, TemplateTransformation}
import cdmia.core.categorytheory.functor.Functor
import java.sql._
	
@main def main(args: String*) =
	// Execute with "scala -classpath CDMiA-DataWrapper-0.2.0.jar:postgresql-42.7.3.jar benchmark_execution_postgres.scala"
	
	val file = java.io.PrintWriter("execution_time.csv")
	
	// Conditions of experiments
	val numberOfRepetitions: Int = 10
	
	// Get PG connection
	Class.forName("org.postgresql.Driver")
	val pgBase: String = "" // Replace by your own value
	val pgUser: String = "" // Replace by your own value
	val pgPassword: String = "" // Replace by your own value
	val pgURL: String = s"jdbc:postgresql://localhost:5432/${pgBase}?user=${pgUser}&password=${pgPassword}"
	val connection: Connection = DriverManager.getConnection(pgURL)
	val pgSource = new RelationalJDBCSource("benchmark", connection)
	
	// Get schemas
	val schemaMetadata = connection.getMetaData
	var schemas: List[String] = {
		val rs = schemaMetadata.getSchemas
		var l = List[String]()
		while (rs.next()) {
			val schema = rs.getString("TABLE_SCHEM")
			if (schema.startsWith("benchmark")) {
				l :+= schema
			}
		}
		l
	}
	
	val template = relationalToPropertyGraph()
	file.write(s"schema," + (for (i <- 0 until numberOfRepetitions) yield {
		s"execution_${i}_schema_loading_ms,execution_${i}_migration_application_ms"
	}).mkString(",") + "\n")
	file.flush()
	for (schema <- schemas) {
		val line = s"$schema," + (for (i <- 0 until numberOfRepetitions) yield {
			val result = bench(schema, pgSource, template)
			s"${result.schemaLoadingMs},${result.migrationApplicationMs}"
		}).mkString(",")
		println(line)
		file.write(line + "\n")
		file.flush()
	}
	
	file.close()

case class ExecutionTime(schemaLoadingMs: Long, migrationApplicationMs: Long)

def bench(schema: String, pgSource: RelationalJDBCSource, transformation: TemplateTransformation): ExecutionTime = {
	val schemaLoadingBeginTime = System.currentTimeMillis()
	val categoricalSchema = pgSource.getRelationalSchema(Some(List(schema)), None)
	val schemaLoadingTimeMs = System.currentTimeMillis() - schemaLoadingBeginTime
	
	val migrationApplicationBeginTime = System.currentTimeMillis()
	val result = new SchemaModelTransformation(categoricalSchema, transformation)
	val migrationApplicationTimeMs = System.currentTimeMillis() - migrationApplicationBeginTime
	
	ExecutionTime(schemaLoadingTimeMs, migrationApplicationTimeMs)
}	
	
def relationalToPropertyGraph(): TemplateTransformation = {
	new TemplateTransformation("Relational_to_PropertyGraph", RelationalModel, PropertyGraphModel,
		new Functor("Relational_to_PropertyGraph", RelationalModel.category, PropertyGraphModel.category,
			List(
			  RelationalModel.table ~> PropertyGraphModel.vertex,
			  RelationalModel.attributeDomain ~> PropertyGraphModel.typedAttribute,
			  RelationalModel.primaryKeyDomain ~> PropertyGraphModel.vertex,
			  RelationalModel.foreignKey ~> PropertyGraphModel.edge,
			  RelationalModel.boolean ~> PropertyGraphModel.boolean,
			  RelationalModel.number ~> PropertyGraphModel.number,
			  RelationalModel.string ~> PropertyGraphModel.string,
			  RelationalModel.date ~> PropertyGraphModel.date
			),
			List(
			  RelationalModel.attribute ~> PropertyGraphModel.vertexAttribute,
			  RelationalModel.primaryKey ~> PropertyGraphModel.vertex.identityMorphism,
			  RelationalModel.inversePrimaryKey ~> PropertyGraphModel.vertex.identityMorphism,
			  RelationalModel.primaryKeyAttribute ~> PropertyGraphModel.vertexAttribute,
			  RelationalModel.primaryKeyAttributePartOfForeignKey ~> PropertyGraphModel.vertexAttribute,
			  RelationalModel.to ~> PropertyGraphModel.out,
			  RelationalModel.from ~> PropertyGraphModel.in,
			  RelationalModel.foreignKeyConstraint ~> PropertyGraphModel.vertex.identityMorphism,
			  RelationalModel.foreignKeyAttribute ~> PropertyGraphModel.vertexAttribute,
			  RelationalModel.booleanAttribute ~> PropertyGraphModel.booleanType,
			  RelationalModel.numberAttribute ~> PropertyGraphModel.numberType,
			  RelationalModel.stringAttribute ~> PropertyGraphModel.stringType,
			  RelationalModel.dateAttribute ~> PropertyGraphModel.dateType
			)
		)
	)
}
