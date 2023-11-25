package cdmia.datawrapper.schema.relational

import cdmia.datawrapper.model.relational.RelationalModel._
import org.scalatest.funsuite.*

class RelationalSchemaBuilderTest extends AnyFunSuite {
  test("An empty schema can be built") {
    assertResult(true)(RelationalSchemaBuilder("test").build().isInstanceOf[RelationalSchema])
  }

  test("A schema with a table can be built") {
    val builder = RelationalSchemaBuilder("test")
      .addTable(new Table("t"))
    assertResult(true)(builder.build().isInstanceOf[RelationalSchema])
  }

  test("A schema with a table and simple attributes can be built") {
    val table = new Table("t")
    val builder = RelationalSchemaBuilder("test")
      .addTable(table)
      .addAttribute(new Attribute("a1", table, StringType))
      .addAttribute(new Attribute("a2", table, NumberType))
      .addAttribute(new Attribute("a3", table, BooleanType))
      .addAttribute(new Attribute("a4", table, DateType))
    assertResult(true)(builder.build().isInstanceOf[RelationalSchema])
  }

  test("Adding an attribute without the corresponding table throws an exception") {
    val table = new Table("t")
    val dataType = StringType
    val builder = RelationalSchemaBuilder("test")
      .addTable(table)
    assertThrows[IllegalArgumentException](builder.addAttribute(new Attribute("a", new Table("t"), dataType)))
  }

  test("A schema with a table and a primary key attributes can be built") {
    val table = new Table("t")
    val dataType = StringType
    val builder = RelationalSchemaBuilder("test")
      .addTable(table)
      .addPrimaryKeyAttribute(new PrimaryKeyAttribute("a", table, dataType))
    assertResult(true)(builder.build().isInstanceOf[RelationalSchema])
    assertResult(true)(builder.addPrimaryKeyAttribute(new PrimaryKeyAttribute("a2", table, dataType)).build().isInstanceOf[RelationalSchema])
  }

  test("A schema with a table and a composed primary key attributes can be built") {
    val table = new Table("t")
    val dataType = StringType
    val builder = RelationalSchemaBuilder("test")
      .addTable(table)
      .addComposedPrimaryKeyAttribute(new ComposedPrimaryKeyAttribute("a", table, List[IndividualComposedPrimaryKeyAttribute](IndividualComposedPrimaryKeyAttribute("a1", dataType), IndividualComposedPrimaryKeyAttribute("a2", dataType))))
    assertResult(true)(builder.build().isInstanceOf[RelationalSchema])
    assertResult(true)(builder.addComposedPrimaryKeyAttribute(new ComposedPrimaryKeyAttribute("a", table, List[IndividualComposedPrimaryKeyAttribute](IndividualComposedPrimaryKeyAttribute("a1", dataType), IndividualComposedPrimaryKeyAttribute("a2", dataType)))).build().isInstanceOf[RelationalSchema])
  }

  test("Adding a primary key without the corresponding table throws an exception") {
    val table = new Table("t")
    val dataType = StringType
    val builder = RelationalSchemaBuilder("test")
      .addTable(table)
    assertThrows[IllegalArgumentException](builder.addPrimaryKeyAttribute(new PrimaryKeyAttribute("a", new Table("t"), dataType)))
  }

  test("A schema with a table and a foreign key attribute can be built") {
    val table = new Table("t")
    val dataType = StringType
    val primaryKey = new PrimaryKeyAttribute("a", table, dataType)
    val builder = RelationalSchemaBuilder("test")
      .addTable(table)
      .addPrimaryKeyAttribute(primaryKey)
      .addForeignKeyAttribute(new ForeignKeyAttribute("f", table, primaryKey))
    assertResult(true)(builder.build().isInstanceOf[RelationalSchema])
  }

  test("A schema with a table and a composed foreign key attribute can be built") {
    val table = new Table("t")
    val dataType = StringType
    val attribute1 = IndividualComposedPrimaryKeyAttribute("a1", dataType)
    val attribute2 = IndividualComposedPrimaryKeyAttribute("a2", dataType)
    val primaryKey = new ComposedPrimaryKeyAttribute("a", table, List(attribute1, attribute2))
    val builder = RelationalSchemaBuilder("test")
      .addTable(table)
      .addComposedPrimaryKeyAttribute(primaryKey)
      .addComposedForeignKeyAttribute(new ComposedForeignKeyAttribute("f", table, primaryKey, Map(attribute1 -> "la1", attribute2 -> "la2")))
    assertResult(true)(builder.build().isInstanceOf[RelationalSchema])
  }

  test("Adding a foreign key without the corresponding table or primary key throws an exception") {
    val table = new Table("t")
    val dataType = StringType
    val primaryKey = new PrimaryKeyAttribute("a", table, dataType)
    val builder = RelationalSchemaBuilder("test")
      .addTable(table)
      .addPrimaryKeyAttribute(primaryKey)
    assertThrows[IllegalArgumentException](builder.addForeignKeyAttribute(new ForeignKeyAttribute("f", new Table("t"), primaryKey)))
    assertThrows[IllegalArgumentException](builder.addForeignKeyAttribute(new ForeignKeyAttribute("f", table, new PrimaryKeyAttribute("a", table, dataType))))
  }
}
