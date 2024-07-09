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
      .addAttribute(Attribute("a1", table, StringType))
      .addAttribute(Attribute("a2", table, NumberType))
      .addAttribute(Attribute("a3", table, BooleanType))
      .addAttribute(Attribute("a4", table, DateType))
    assertResult(true)(builder.build().isInstanceOf[RelationalSchema])
  }

  test("Adding an attribute without the corresponding table throws an exception") {
    val table = new Table("t")
    val dataType = StringType
    val builder = RelationalSchemaBuilder("test")
      .addTable(table)
    assertThrows[IllegalArgumentException](builder.addAttribute(Attribute("a", new Table("t1"), dataType)))
  }

  test("A schema with a table and a primary key attribute can be built") {
    val table = new Table("t")
    val dataType = StringType
    val pkAttribute1 = Attribute("a1", table, dataType)
    val pkAttribute2 = Attribute("a2", table, dataType)
    val builder = RelationalSchemaBuilder("test")
      .addTable(table)
      .addAttribute(pkAttribute1)
      .addAttribute(pkAttribute2)
      .addPrimaryKey(new PrimaryKey("pk", table, List(pkAttribute1)))
    assertResult(true)(builder.build().isInstanceOf[RelationalSchema])
    assertResult(true)(builder.addPrimaryKey(new PrimaryKey("pk2", table, List(pkAttribute2))).build().isInstanceOf[RelationalSchema])
  }

  test("A schema with a table and a composed primary key can be built") {
    val table = new Table("t")
    val dataType = StringType
    val pkAttribute1 = Attribute("a1", table, dataType)
    val pkAttribute2 = Attribute("a2", table, dataType)
    val pkAttribute3 = Attribute("a3", table, dataType)
    val pkAttribute4 = Attribute("a4", table, dataType)
    val builder = RelationalSchemaBuilder("test")
      .addTable(table)
      .addAttribute(pkAttribute1)
      .addAttribute(pkAttribute2)
      .addAttribute(pkAttribute3)
      .addAttribute(pkAttribute4)
      .addPrimaryKey(new PrimaryKey("pk", table, List(pkAttribute1, pkAttribute2)))
    assertResult(true)(builder.build().isInstanceOf[RelationalSchema])
    assertResult(true)(builder.addPrimaryKey(new PrimaryKey("pk2", table, List(pkAttribute3, pkAttribute4))).build().isInstanceOf[RelationalSchema])
    assertResult(true)(builder.addPrimaryKey(new PrimaryKey("pk2", table, List(pkAttribute1, pkAttribute4))).build().isInstanceOf[RelationalSchema])
  }

  test("Adding a primary key without the corresponding table throws an exception") {
    val table = new Table("t")
    val table2 = new Table("t")
    val dataType = StringType
    val pkAttribute1 = Attribute("a1", table, dataType)
    val builder = RelationalSchemaBuilder("test")
      .addTable(table)
      .addAttribute(pkAttribute1)
    assertThrows[IllegalArgumentException](builder.addPrimaryKey(new PrimaryKey("pk", table2, List(pkAttribute1))))
    assertThrows[IllegalArgumentException](
      builder
        .addTable(table2)
        .addPrimaryKey(new PrimaryKey("pk", table2, List(pkAttribute1)))
    )
  }

  test("A schema with a table and a foreign key attribute can be built") {
    val table = new Table("t")
    val table2 = new Table("t2")
    val dataType = StringType
    val primaryKeyAttribute = Attribute("a", table, dataType)
    val foreignKeyAttribute = Attribute("fka", table2, dataType)
    val primaryKey = new PrimaryKey("pk", table, List(primaryKeyAttribute))
    val builder = RelationalSchemaBuilder("test")
      .addTable(table)
      .addTable(table2)
      .addAttribute(primaryKeyAttribute)
      .addAttribute(foreignKeyAttribute)
      .addPrimaryKey(primaryKey)
      .addForeignKey(new ForeignKey("f", table2, primaryKey, Map(foreignKeyAttribute -> primaryKeyAttribute)))
    assertResult(true)(builder.build().isInstanceOf[RelationalSchema])
  }

  test("A schema with a table and a composed foreign key attribute can be built") {
    val table = new Table("t")
    val table2 = new Table("t2")
    val dataType = StringType
    val attribute1 = Attribute("a1", table, dataType)
    val attribute2 = Attribute("a2", table, dataType)
    val fkAttribute1 = Attribute("fka1", table2, dataType)
    val fkAttribute2 = Attribute("fka2", table2, dataType)
    val primaryKey = new PrimaryKey("a", table, List(attribute1, attribute2))
    val builder = RelationalSchemaBuilder("test")
      .addTable(table)
      .addTable(table2)
      .addAttribute(attribute1)
      .addAttribute(attribute2)
      .addAttribute(fkAttribute1)
      .addAttribute(fkAttribute2)
      .addPrimaryKey(primaryKey)
      .addForeignKey(new ForeignKey("f", table2, primaryKey, Map(fkAttribute1 -> attribute1, fkAttribute2 -> attribute2)))
    assertResult(true)(builder.build().isInstanceOf[RelationalSchema])
  }

  test("Adding a foreign key without the corresponding table or primary key throws an exception") {
    val table = new Table("t")
    val table2 = new Table("t2")
    val dataType = StringType
    val primaryKeyAttribute = Attribute("a", table, dataType)
    val foreignKeyAttribute = Attribute("a", table2, dataType)
    val primaryKey = new PrimaryKey("pk", table, List(primaryKeyAttribute))
    val builder = RelationalSchemaBuilder("test")
      .addTable(table)
      .addTable(table2)
      .addAttribute(primaryKeyAttribute)
      .addAttribute(foreignKeyAttribute)
      .addPrimaryKey(primaryKey)
    assertThrows[IllegalArgumentException](builder.addForeignKey(new ForeignKey("f", new Table("t"), primaryKey, Map(foreignKeyAttribute -> primaryKeyAttribute))))
    assertThrows[IllegalArgumentException](builder.addForeignKey(new ForeignKey("f", table, new PrimaryKey("pk2", table, List(primaryKeyAttribute)), Map(foreignKeyAttribute -> primaryKeyAttribute))))
  }
}
