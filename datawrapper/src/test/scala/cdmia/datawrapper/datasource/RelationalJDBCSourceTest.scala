package cdmia.datawrapper.datasource

import cdmia.core.categorytheory.Object
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.Morphism
import cdmia.datawrapper.model.relational.RelationalModel
import cdmia.datawrapper.model.graph.PropertyGraphModel
import cdmia.datawrapper.modeltransformation.{SchemaModelTransformation, TemplateTransformation}
import cdmia.datawrapper.schema.relational.RelationalSchema

import java.sql.{Connection, DriverManager}
import java.util.Properties
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.*
import io.zonky.test.db.postgres.embedded.EmbeddedPostgres

class RelationalJDBCSourceTest extends AnyFunSuite with BeforeAndAfterAll {
  var embeddedPostgres: EmbeddedPostgres = null
  var connection: Connection = null
  case class ExpectedMorphism(name: String, domain: Object, codomain: Object)

  override def beforeAll(): Unit = {
    embeddedPostgres = EmbeddedPostgres.start()
    connection = embeddedPostgres.getPostgresDatabase.getConnection
  }

  override def afterAll(): Unit = {
    embeddedPostgres.close()
    cdmia.core.categorytheory.Config.disableRequire = false
    cdmia.datawrapper.Config.disableRequire = false
  }

  /**
   * test 1
   */
  test("should build a schema with a table having only simple attributes") {
    val stmt = connection.createStatement()
    stmt.executeUpdate("CREATE SCHEMA test1")
    stmt.executeUpdate("CREATE TABLE test1.table1(att1 TEXT, att2 INTEGER)")
    val postgreSQLSource = new RelationalJDBCSource("test1", connection)
    val schema = postgreSQLSource.getRelationalSchema(Some(List("test1")), None)
    assertResult(true)(schema.isInstanceOf[RelationalSchema])

    // Check objects
    val expectedObjectNames = List[String]("test1.table1", "att1_domain", "att2_domain", "number", "string")
    val obtainedObjects: Map[String, Object] = (for (obj <- schema.category.objects) yield obj.name -> obj).toMap
    checkObjects(expectedObjectNames, obtainedObjects)

    // Check morphisms
    val expectedMorphisms = List[ExpectedMorphism](
      ExpectedMorphism("att1", obtainedObjects("test1.table1"), obtainedObjects("att1_domain")),
      ExpectedMorphism("att1_type", obtainedObjects("att1_domain"), obtainedObjects("string")),
      ExpectedMorphism("att2", obtainedObjects("test1.table1"), obtainedObjects("att2_domain")),
      ExpectedMorphism("att2_type", obtainedObjects("att2_domain"), obtainedObjects("number"))
    )
    val obtainedMorphisms: Map[String, Morphism] = (for (morphism <- schema.category.morphisms) yield morphism.name -> morphism).toMap
    checkMorphisms(expectedMorphisms, obtainedMorphisms)

    // Check object transformations
    val expectedObjectTransformations = Map[Object, Object](
      obtainedObjects("test1.table1") -> RelationalModel.table,
      obtainedObjects("att1_domain") -> RelationalModel.attributeDomain,
      obtainedObjects("att2_domain") -> RelationalModel.attributeDomain,
      obtainedObjects("number") -> RelationalModel.number,
      obtainedObjects("string") -> RelationalModel.string
    )
    checkObjectTransformations(schema.functorTowardsModel, expectedObjectTransformations)

    // Check morphism transformations
    val expectedMorphismTransformations = Map[Morphism, Morphism](
      obtainedMorphisms("att1") -> RelationalModel.attribute,
      obtainedMorphisms("att1_type") -> RelationalModel.stringAttribute,
      obtainedMorphisms("att2") -> RelationalModel.attribute,
      obtainedMorphisms("att2_type") -> RelationalModel.numberAttribute
    )
    checkMorphismTransformations(schema.functorTowardsModel, expectedMorphismTransformations)
  }

  /**
   * test underscore
   */
  test("should build a schema with underscore in name") {
    val stmt = connection.createStatement()
    stmt.executeUpdate("CREATE SCHEMA test_1")
    stmt.executeUpdate("CREATE TABLE test_1.table1(att1 TEXT, att2 INTEGER)")
    val postgreSQLSource = new RelationalJDBCSource("test_1", connection)
    val schema = postgreSQLSource.getRelationalSchema(Some(List("test_1")), None)
    assertResult(true)(schema.isInstanceOf[RelationalSchema])

    // Check objects
    val expectedObjectNames = List[String]("test_1.table1", "att1_domain", "att2_domain", "number", "string")
    val obtainedObjects: Map[String, Object] = (for (obj <- schema.category.objects) yield obj.name -> obj).toMap
    checkObjects(expectedObjectNames, obtainedObjects)

    // Check morphisms
    val expectedMorphisms = List[ExpectedMorphism](
      ExpectedMorphism("att1", obtainedObjects("test_1.table1"), obtainedObjects("att1_domain")),
      ExpectedMorphism("att1_type", obtainedObjects("att1_domain"), obtainedObjects("string")),
      ExpectedMorphism("att2", obtainedObjects("test_1.table1"), obtainedObjects("att2_domain")),
      ExpectedMorphism("att2_type", obtainedObjects("att2_domain"), obtainedObjects("number"))
    )
    val obtainedMorphisms: Map[String, Morphism] = (for (morphism <- schema.category.morphisms) yield morphism.name -> morphism).toMap
    checkMorphisms(expectedMorphisms, obtainedMorphisms)

    // Check object transformations
    val expectedObjectTransformations = Map[Object, Object](
      obtainedObjects("test_1.table1") -> RelationalModel.table,
      obtainedObjects("att1_domain") -> RelationalModel.attributeDomain,
      obtainedObjects("att2_domain") -> RelationalModel.attributeDomain,
      obtainedObjects("number") -> RelationalModel.number,
      obtainedObjects("string") -> RelationalModel.string
    )
    checkObjectTransformations(schema.functorTowardsModel, expectedObjectTransformations)

    // Check morphism transformations
    val expectedMorphismTransformations = Map[Morphism, Morphism](
      obtainedMorphisms("att1") -> RelationalModel.attribute,
      obtainedMorphisms("att1_type") -> RelationalModel.stringAttribute,
      obtainedMorphisms("att2") -> RelationalModel.attribute,
      obtainedMorphisms("att2_type") -> RelationalModel.numberAttribute
    )
    checkMorphismTransformations(schema.functorTowardsModel, expectedMorphismTransformations)
  }

  /**
   * test 2
   */
  test("should build a schema with a table having simple attributes and a primary key") {
    val stmt = connection.createStatement()
    stmt.executeUpdate("CREATE SCHEMA test2")
    stmt.executeUpdate("CREATE TABLE test2.table1(att1 TEXT, att2 INTEGER, CONSTRAINT pk PRIMARY KEY(att2))")
    val postgreSQLSource = new RelationalJDBCSource("test2", connection)
    val schema = postgreSQLSource.getRelationalSchema(Some(List("test2")), None)
    assertResult(true)(schema.isInstanceOf[RelationalSchema])

    // Check objects
    val expectedObjectNames = List[String]("test2.table1", "att1_domain", "pk_primary_key_domain", "att2_domain", "number", "string")
    val obtainedObjects: Map[String, Object] = (for (obj <- schema.category.objects) yield obj.name -> obj).toMap
    checkObjects(expectedObjectNames, obtainedObjects)

    // Check morphisms
    val expectedMorphisms = List[ExpectedMorphism](
      ExpectedMorphism("att1", obtainedObjects("test2.table1"), obtainedObjects("att1_domain")),
      ExpectedMorphism("att1_type", obtainedObjects("att1_domain"), obtainedObjects("string")),
      ExpectedMorphism("pk", obtainedObjects("test2.table1"), obtainedObjects("pk_primary_key_domain")),
      ExpectedMorphism("pk_inverse", obtainedObjects("pk_primary_key_domain"), obtainedObjects("test2.table1")),
      ExpectedMorphism("att2", obtainedObjects("pk_primary_key_domain"), obtainedObjects("att2_domain")),
      ExpectedMorphism("att2_type", obtainedObjects("att2_domain"), obtainedObjects("number"))
    )
    val obtainedMorphisms: Map[String, Morphism] = (for (morphism <- schema.category.morphisms) yield morphism.name -> morphism).toMap
    checkMorphisms(expectedMorphisms, obtainedMorphisms)

    // Check object transformations
    val expectedObjectTransformations = Map[Object, Object](
      obtainedObjects("test2.table1") -> RelationalModel.table,
      obtainedObjects("att1_domain") -> RelationalModel.attributeDomain,
      obtainedObjects("pk_primary_key_domain") -> RelationalModel.primaryKeyDomain,
      obtainedObjects("att2_domain") -> RelationalModel.attributeDomain,
      obtainedObjects("number") -> RelationalModel.number,
      obtainedObjects("string") -> RelationalModel.string
    )
    checkObjectTransformations(schema.functorTowardsModel, expectedObjectTransformations)

    // Check morphism transformations
    val expectedMorphismTransformations = Map[Morphism, Morphism](
      obtainedMorphisms("att1") -> RelationalModel.attribute,
      obtainedMorphisms("att1_type") -> RelationalModel.stringAttribute,
      obtainedMorphisms("pk") -> RelationalModel.primaryKey,
      obtainedMorphisms("pk_inverse") -> RelationalModel.inversePrimaryKey,
      obtainedMorphisms("att2") -> RelationalModel.primaryKeyAttribute,
      obtainedMorphisms("att2_type") -> RelationalModel.numberAttribute
    )
    checkMorphismTransformations(schema.functorTowardsModel, expectedMorphismTransformations)
  }

  /**
   * test 3
   */
  test("should build a schema with a table having simple attributes and a composed primary key") {
    val stmt = connection.createStatement()
    stmt.executeUpdate("CREATE SCHEMA test3")
    stmt.executeUpdate("CREATE TABLE test3.table1(att1 TEXT, att2 INTEGER, att3 INTEGER, CONSTRAINT pk PRIMARY KEY(att2, att3))")
    val postgreSQLSource = new RelationalJDBCSource("test3", connection)
    val schema = postgreSQLSource.getRelationalSchema(Some(List("test3")), None)
    assertResult(true)(schema.isInstanceOf[RelationalSchema])

    // Check objects
    val expectedObjectNames = List[String](
      "test3.table1",
      "att1_domain",
      "pk_primary_key_domain",
      "att2_domain",
      "att3_domain",
      "number",
      "string"
    )
    val obtainedObjects: Map[String, Object] = (for (obj <- schema.category.objects) yield obj.name -> obj).toMap
    checkObjects(expectedObjectNames, obtainedObjects)

    // Check morphisms
    val expectedMorphisms = List[ExpectedMorphism](
      ExpectedMorphism("att1", obtainedObjects("test3.table1"), obtainedObjects("att1_domain")),
      ExpectedMorphism("att1_type", obtainedObjects("att1_domain"), obtainedObjects("string")),
      ExpectedMorphism("pk", obtainedObjects("test3.table1"), obtainedObjects("pk_primary_key_domain")),
      ExpectedMorphism("pk_inverse", obtainedObjects("pk_primary_key_domain"), obtainedObjects("test3.table1")),
      ExpectedMorphism("att2", obtainedObjects("pk_primary_key_domain"), obtainedObjects("att2_domain")),
      ExpectedMorphism("att2_type", obtainedObjects("att2_domain"), obtainedObjects("number")),
      ExpectedMorphism("att3", obtainedObjects("pk_primary_key_domain"), obtainedObjects("att3_domain")),
      ExpectedMorphism("att3_type", obtainedObjects("att3_domain"), obtainedObjects("number"))
    )
    val obtainedMorphisms: Map[String, Morphism] = (for (morphism <- schema.category.morphisms) yield morphism.name -> morphism).toMap
    checkMorphisms(expectedMorphisms, obtainedMorphisms)

    // Check object transformations
    val expectedObjectTransformations = Map[Object, Object](
      obtainedObjects("test3.table1") -> RelationalModel.table,
      obtainedObjects("att1_domain") -> RelationalModel.attributeDomain,
      obtainedObjects("pk_primary_key_domain") -> RelationalModel.primaryKeyDomain,
      obtainedObjects("att2_domain") -> RelationalModel.attributeDomain,
      obtainedObjects("att3_domain") -> RelationalModel.attributeDomain,
      obtainedObjects("number") -> RelationalModel.number,
      obtainedObjects("string") -> RelationalModel.string
    )
    checkObjectTransformations(schema.functorTowardsModel, expectedObjectTransformations)

    // Check morphism transformations
    val expectedMorphismTransformations = Map[Morphism, Morphism](
      obtainedMorphisms("att1") -> RelationalModel.attribute,
      obtainedMorphisms("att1_type") -> RelationalModel.stringAttribute,
      obtainedMorphisms("pk") -> RelationalModel.primaryKey,
      obtainedMorphisms("pk_inverse") -> RelationalModel.inversePrimaryKey,
      obtainedMorphisms("att2") -> RelationalModel.primaryKeyAttribute,
      obtainedMorphisms("att2_type") -> RelationalModel.numberAttribute,
      obtainedMorphisms("att3") -> RelationalModel.primaryKeyAttribute,
      obtainedMorphisms("att3_type") -> RelationalModel.numberAttribute
    )
    checkMorphismTransformations(schema.functorTowardsModel, expectedMorphismTransformations)
  }

  /**
   * test 4
   */
  test("should build a schema with two table having simple attributes, a primary key and a foreign key") {
    val stmt = connection.createStatement()
    stmt.executeUpdate("CREATE SCHEMA test4")
    stmt.executeUpdate("CREATE TABLE test4.table1(att1 TEXT, att2 INTEGER, CONSTRAINT pk_table1 PRIMARY KEY(att2))")
    stmt.executeUpdate("CREATE TABLE test4.table2(att3 TEXT, att4 INTEGER, CONSTRAINT pk_table2 PRIMARY KEY(att3), CONSTRAINT fk FOREIGN KEY(att4) REFERENCES test4.table1(att2))")
    val postgreSQLSource = new RelationalJDBCSource("test4", connection)
    val schema = postgreSQLSource.getRelationalSchema(Some(List("test4")), None)
    assertResult(true)(schema.isInstanceOf[RelationalSchema])

    // Check objects
    val expectedObjectNames = List[String](
      "test4.table1",
      "test4.table2",
      "att1_domain",
      "pk_table1_primary_key_domain",
      "att2_domain",
      "pk_table2_primary_key_domain",
      "att3_domain",
      "fk_foreign_key",
      "number",
      "string"
    )
    val obtainedObjects: Map[String, Object] = (for (obj <- schema.category.objects) yield obj.name -> obj).toMap
    checkObjects(expectedObjectNames, obtainedObjects)

    // Check morphisms
    val expectedMorphisms = List[ExpectedMorphism](
      ExpectedMorphism("att1", obtainedObjects("test4.table1"), obtainedObjects("att1_domain")),
      ExpectedMorphism("att1_type", obtainedObjects("att1_domain"), obtainedObjects("string")),
      ExpectedMorphism("pk_table1", obtainedObjects("test4.table1"), obtainedObjects("pk_table1_primary_key_domain")),
      ExpectedMorphism("pk_table1_inverse", obtainedObjects("pk_table1_primary_key_domain"), obtainedObjects("test4.table1")),
      ExpectedMorphism("att2", obtainedObjects("pk_table1_primary_key_domain"), obtainedObjects("att2_domain")),
      ExpectedMorphism("att2_type", obtainedObjects("att2_domain"), obtainedObjects("number")),
      ExpectedMorphism("pk_table2", obtainedObjects("test4.table2"), obtainedObjects("pk_table2_primary_key_domain")),
      ExpectedMorphism("pk_table2_inverse", obtainedObjects("pk_table2_primary_key_domain"), obtainedObjects("test4.table2")),
      ExpectedMorphism("att3", obtainedObjects("pk_table2_primary_key_domain"), obtainedObjects("att3_domain")),
      ExpectedMorphism("att3_type", obtainedObjects("att3_domain"), obtainedObjects("string")),
      ExpectedMorphism("fk_to", obtainedObjects("fk_foreign_key"), obtainedObjects("pk_table1_primary_key_domain")),
      ExpectedMorphism("fk_from", obtainedObjects("fk_foreign_key"), obtainedObjects("test4.table2")),
      ExpectedMorphism("fk", obtainedObjects("test4.table2"), obtainedObjects("pk_table1_primary_key_domain")),
      ExpectedMorphism("att4", obtainedObjects("test4.table2"), obtainedObjects("att2_domain"))
    )
    val obtainedMorphisms: Map[String, Morphism] = (for (morphism <- schema.category.morphisms) yield morphism.name -> morphism).toMap
    checkMorphisms(expectedMorphisms, obtainedMorphisms)

    // Check object transformations
    val expectedObjectTransformations = Map[Object, Object](
      obtainedObjects("test4.table1") -> RelationalModel.table,
      obtainedObjects("test4.table2") -> RelationalModel.table,
      obtainedObjects("att1_domain") -> RelationalModel.attributeDomain,
      obtainedObjects("pk_table1_primary_key_domain") -> RelationalModel.primaryKeyDomain,
      obtainedObjects("att2_domain") -> RelationalModel.attributeDomain,
      obtainedObjects("pk_table2_primary_key_domain") -> RelationalModel.primaryKeyDomain,
      obtainedObjects("att3_domain") -> RelationalModel.attributeDomain,
      obtainedObjects("fk_foreign_key") -> RelationalModel.foreignKey,
      obtainedObjects("number") -> RelationalModel.number,
      obtainedObjects("string") -> RelationalModel.string
    )
    checkObjectTransformations(schema.functorTowardsModel, expectedObjectTransformations)

    // Check morphism transformations
    val expectedMorphismTransformations = Map[Morphism, Morphism](
      obtainedMorphisms("att1") -> RelationalModel.attribute,
      obtainedMorphisms("att1_type") -> RelationalModel.stringAttribute,
      obtainedMorphisms("pk_table1") -> RelationalModel.primaryKey,
      obtainedMorphisms("pk_table1_inverse") -> RelationalModel.inversePrimaryKey,
      obtainedMorphisms("att2") -> RelationalModel.primaryKeyAttribute,
      obtainedMorphisms("att2_type") -> RelationalModel.numberAttribute,
      obtainedMorphisms("pk_table2") -> RelationalModel.primaryKey,
      obtainedMorphisms("pk_table2_inverse") -> RelationalModel.inversePrimaryKey,
      obtainedMorphisms("att3") -> RelationalModel.primaryKeyAttribute,
      obtainedMorphisms("att3_type") -> RelationalModel.stringAttribute,
      obtainedMorphisms("fk_to") -> RelationalModel.to,
      obtainedMorphisms("fk_from") -> RelationalModel.from,
      obtainedMorphisms("fk") -> RelationalModel.foreignKeyConstraint,
      obtainedMorphisms("att4") -> RelationalModel.foreignKeyAttribute
    )
    checkMorphismTransformations(schema.functorTowardsModel, expectedMorphismTransformations)
  }

  /**
   * test 5
   */
  test("should build a schema with two table having simple attributes, a primary key and a composed foreign key") {
    val stmt = connection.createStatement()
    stmt.executeUpdate("CREATE SCHEMA test5")
    stmt.executeUpdate("CREATE TABLE test5.table1(att1 TEXT, att2 INTEGER, CONSTRAINT pk_table1 PRIMARY KEY(att1, att2))")
    stmt.executeUpdate("CREATE TABLE test5.table2(att3 TEXT, att4 INTEGER, CONSTRAINT pk_table2 PRIMARY KEY(att3), CONSTRAINT fk FOREIGN KEY(att3, att4) REFERENCES test5.table1(att1, att2))")
    val postgreSQLSource = new RelationalJDBCSource("test5", connection)
    val schema = postgreSQLSource.getRelationalSchema(Some(List("test5")), None)
    assertResult(true)(schema.isInstanceOf[RelationalSchema])

    // Check objects
    val expectedObjectNames = List[String](
      "test5.table1",
      "test5.table2",
      "att1_domain",
      "pk_table1_primary_key_domain",
      "att2_domain",
      "pk_table2_primary_key_domain",
      "fk_foreign_key",
      "number",
      "string"
    )
    val obtainedObjects: Map[String, Object] = (for (obj <- schema.category.objects) yield obj.name -> obj).toMap
    checkObjects(expectedObjectNames, obtainedObjects)

    // Check morphisms
    val expectedMorphisms = List[ExpectedMorphism](
      ExpectedMorphism("att1", obtainedObjects("pk_table1_primary_key_domain"), obtainedObjects("att1_domain")),
      ExpectedMorphism("att1_type", obtainedObjects("att1_domain"), obtainedObjects("string")),
      ExpectedMorphism("pk_table1", obtainedObjects("test5.table1"), obtainedObjects("pk_table1_primary_key_domain")),
      ExpectedMorphism("pk_table1_inverse", obtainedObjects("pk_table1_primary_key_domain"), obtainedObjects("test5.table1")),
      ExpectedMorphism("att2", obtainedObjects("pk_table1_primary_key_domain"), obtainedObjects("att2_domain")),
      ExpectedMorphism("att2_type", obtainedObjects("att2_domain"), obtainedObjects("number")),
      ExpectedMorphism("pk_table2", obtainedObjects("test5.table2"), obtainedObjects("pk_table2_primary_key_domain")),
      ExpectedMorphism("pk_table2_inverse", obtainedObjects("pk_table2_primary_key_domain"), obtainedObjects("test5.table2")),
      ExpectedMorphism("att3", obtainedObjects("pk_table2_primary_key_domain"), obtainedObjects("att1_domain")),
      ExpectedMorphism("fk_to", obtainedObjects("fk_foreign_key"), obtainedObjects("pk_table1_primary_key_domain")),
      ExpectedMorphism("fk_from", obtainedObjects("fk_foreign_key"), obtainedObjects("test5.table2")),
      ExpectedMorphism("fk", obtainedObjects("test5.table2"), obtainedObjects("pk_table1_primary_key_domain")),
      ExpectedMorphism("att4", obtainedObjects("test5.table2"), obtainedObjects("att2_domain"))
    )
    val obtainedMorphisms: Map[String, Morphism] = (for (morphism <- schema.category.morphisms) yield morphism.name -> morphism).toMap
    checkMorphisms(expectedMorphisms, obtainedMorphisms)

    // Check object transformations
    val expectedObjectTransformations = Map[Object, Object](
      obtainedObjects("test5.table1") -> RelationalModel.table,
      obtainedObjects("test5.table2") -> RelationalModel.table,
      obtainedObjects("att1_domain") -> RelationalModel.attributeDomain,
      obtainedObjects("pk_table1_primary_key_domain") -> RelationalModel.primaryKeyDomain,
      obtainedObjects("att2_domain") -> RelationalModel.attributeDomain,
      obtainedObjects("pk_table2_primary_key_domain") -> RelationalModel.primaryKeyDomain,
      obtainedObjects("fk_foreign_key") -> RelationalModel.foreignKey,
      obtainedObjects("number") -> RelationalModel.number,
      obtainedObjects("string") -> RelationalModel.string
    )
    checkObjectTransformations(schema.functorTowardsModel, expectedObjectTransformations)

    // Check morphism transformations
    val expectedMorphismTransformations = Map[Morphism, Morphism](
      obtainedMorphisms("att1") -> RelationalModel.primaryKeyAttribute,
      obtainedMorphisms("att1_type") -> RelationalModel.stringAttribute,
      obtainedMorphisms("pk_table1") -> RelationalModel.primaryKey,
      obtainedMorphisms("pk_table1_inverse") -> RelationalModel.inversePrimaryKey,
      obtainedMorphisms("att2") -> RelationalModel.primaryKeyAttribute,
      obtainedMorphisms("att2_type") -> RelationalModel.numberAttribute,
      obtainedMorphisms("pk_table2") -> RelationalModel.primaryKey,
      obtainedMorphisms("pk_table2_inverse") -> RelationalModel.inversePrimaryKey,
      obtainedMorphisms("att3") -> RelationalModel.primaryKeyAttributePartOfForeignKey,
      obtainedMorphisms("fk_to") -> RelationalModel.to,
      obtainedMorphisms("fk_from") -> RelationalModel.from,
      obtainedMorphisms("fk") -> RelationalModel.foreignKeyConstraint,
      obtainedMorphisms("att4") -> RelationalModel.foreignKeyAttribute
    )
    checkMorphismTransformations(schema.functorTowardsModel, expectedMorphismTransformations)
  }

  /**
   * Perf test
   */
  test("should run in a reasonable time") {
    val stmt = connection.createStatement()
    stmt.executeUpdate("CREATE SCHEMA testperf")
    for (i <- 0 until 500) {
      stmt.executeUpdate(s"CREATE TABLE testperf.table$i(att1 TEXT PRIMARY KEY, att2 INTEGER, att3 DATE, att4 BOOLEAN, att5 TEXT)")
      if (i > 0) {
        stmt.executeUpdate(s"ALTER TABLE testperf.table$i ADD FOREIGN KEY(att5) REFERENCES testperf.table${i-1}(att1)")
      }
    }
    val postgreSQLSource = new RelationalJDBCSource("testperf", connection)
    val startTime = System.currentTimeMillis()
    val schema = postgreSQLSource.getRelationalSchema(Some(List("testperf")), None)
    info(s"Run in ${System.currentTimeMillis() - startTime}ms")
  }

  /**
   * Check if the obtained objects are the same as the expected objects.
   */
  private def checkObjects(expectedObjectNames: List[String], obtainedObjects: Map[String, Object]): Unit = {
    assertResult(true)(expectedObjectNames.size == obtainedObjects.size)
    assertResult(true)(expectedObjectNames.forall(obtainedObjects.contains))
    assertResult(true)(obtainedObjects.keys.forall(expectedObjectNames.contains))
  }

  /**
   * Check if the obtained morphisms are the same as the expected morphisms.
   */
  private def checkMorphisms(expectedMorphisms: List[ExpectedMorphism], obtainedMorphisms: Map[String, Morphism]): Unit = {
    assertResult(true)(expectedMorphisms.size == obtainedMorphisms.size)
    for (em <- expectedMorphisms) {
      assertResult(true)(obtainedMorphisms.contains(em.name))
      assertResult(em.name)(obtainedMorphisms(em.name).name)
      assertResult(em.domain)(obtainedMorphisms(em.name).domain)
      assertResult(em.codomain)(obtainedMorphisms(em.name).codomain)
    }
  }

  /**
   * Check if the obtained object transformations are the same as the expected object transformations.
   */
  private def checkObjectTransformations(functor: Functor, expectedObjectTransformations: Map[Object, Object]): Unit = {
    for (ot <- functor.objectTransformations) {
      assertResult(expectedObjectTransformations(ot.source))(ot.destination)
    }
  }

  /**
   * Check if the obtained morphism transformations are the same as the expected morphism transformations.
   */
  private def checkMorphismTransformations(functor: Functor, expectedMorphismTransformations: Map[Morphism, Morphism]): Unit = {
    for (mt <- functor.morphismTransformations) {
      assertResult(expectedMorphismTransformations(mt.source))(mt.destination)
    }
  }
}
