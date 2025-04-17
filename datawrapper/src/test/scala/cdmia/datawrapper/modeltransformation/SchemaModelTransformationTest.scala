package cdmia.datawrapper.modeltransformation

import cdmia.core.categorytheory.functor.Functor
import cdmia.datawrapper.model.graph.PropertyGraphModel
import cdmia.datawrapper.model.relational.RelationalModel
import cdmia.datawrapper.schema.relational.RelationalSchemaBuilder
import org.scalatest.funsuite.AnyFunSuite

class SchemaModelTransformationTest extends AnyFunSuite {
  test("Should run in a reasonable time") {
    cdmia.core.categorytheory.Config.disableRequire = true
    cdmia.datawrapper.Config.disableRequire = true
    val nbTables = 100
    val nbAttributes = 20
    val nbForeignKeys = 3
    var relationalBuilder = RelationalSchemaBuilder("test")
    // Add tables
    val tables = (for (i <- 0 until nbTables) yield {
      val t = RelationalModel.Table(s"table$i")
      relationalBuilder = relationalBuilder.addTable(t)
      t
    }).toArray
    // Add attributes
    var tablesAttributes = Map[RelationalModel.Table, List[RelationalModel.Attribute]]()
    val tablesPrimaryKey = (for (table <- tables) yield {
      var pk: RelationalModel.PrimaryKey = null
      val attributes = (for (i <- 0 until nbAttributes) yield {
        val a = RelationalModel.Attribute(s"att$i", table, RelationalModel.StringType)
        relationalBuilder = relationalBuilder.addAttribute(a)
        if (i == 0) {
          pk = RelationalModel.PrimaryKey(s"pk_${table.name}", table, List(a))
          relationalBuilder = relationalBuilder.addPrimaryKey(pk)
        }
        a
      }).toList
      tablesAttributes += table -> attributes
      table -> pk
    }).toMap
    // Add foreign keys
    for (t <- tables.indices) {
      val table = tables(t)
      for (i <- 0 until nbForeignKeys) {
        val referencedTable = tables((t + i + 1) % nbTables)
        val mapping = Map[RelationalModel.Attribute, RelationalModel.Attribute](tablesAttributes(table)(i + 1) -> tablesPrimaryKey(referencedTable).attributes.head)
        val fk = RelationalModel.ForeignKey(s"fk_${table.name}_$i", table, tablesPrimaryKey(referencedTable), mapping)
        relationalBuilder = relationalBuilder.addForeignKey(fk)
      }
    }
    val schema = relationalBuilder.build()
    val startTime = System.currentTimeMillis()
    new SchemaModelTransformation(schema, templateRelationalToPg)
    val executionTime = System.currentTimeMillis() - startTime
    //info(s"Run in ${executionTime}ms")
    System.out.println(s"Run in ${executionTime}ms")
    assertResult(true)(executionTime < 1000)
  }

  val templateRelationalToPg = new TemplateTransformation("Relational_to_PropertyGraph", RelationalModel, PropertyGraphModel,
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
