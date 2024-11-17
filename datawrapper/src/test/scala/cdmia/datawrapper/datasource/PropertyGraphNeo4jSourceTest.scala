package cdmia.datawrapper.datasource

import apoc.refactor.GraphRefactoring
import org.neo4j.configuration.GraphDatabaseSettings
import org.neo4j.configuration.connectors.{BoltConnector, HttpConnector}
import org.neo4j.dbms.api.{DatabaseManagementService, DatabaseManagementServiceBuilder}
import org.neo4j.driver.AuthTokens
import org.neo4j.exceptions.KernelException
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.kernel.api.procedure.GlobalProcedures
import org.neo4j.kernel.internal.GraphDatabaseAPI
import apoc.coll.Coll
import apoc.convert.Json
import apoc.create.Create
import apoc.load.LoadJson
import apoc.load.Xml
import apoc.meta.Meta
import apoc.path.PathExplorer
import cdmia.core.categorytheory.Object
import cdmia.core.categorytheory.morphism.Morphism
import cdmia.datawrapper.model.graph.PropertyGraphModel
import org.neo4j.driver.{Driver, GraphDatabase}
import org.scalatest.BeforeAndAfterAll

import java.io.File
import scala.reflect.io.Directory

class PropertyGraphNeo4jSourceTest extends DatasourceTest with BeforeAndAfterAll {
  var dbManagement: DatabaseManagementService = null
  var graphDb: GraphDatabaseService = null
  val DATABASE_NAME = "test-neo4j-source"
  override def beforeAll(): Unit = {
    dbManagement = new DatabaseManagementServiceBuilder(new File(DATABASE_NAME).toPath)
      .setConfig(BoltConnector.enabled, true)
      .setConfig(BoltConnector.encryption_level, BoltConnector.EncryptionLevel.DISABLED)
      .setConfig(GraphDatabaseSettings.auth_enabled, false)
      .setConfig(HttpConnector.enabled, true).setConfig(GraphDatabaseSettings.procedure_unrestricted, java.util.List.of("apoc.*"))
      .build()
    // Init APOC
    graphDb = dbManagement.database(GraphDatabaseSettings.DEFAULT_DATABASE_NAME)
    val procedures = graphDb.asInstanceOf[GraphDatabaseAPI]
      .getDependencyResolver
      .resolveDependency(classOf[GlobalProcedures])
    val apocProcedures = java.util.List.of(classOf[Coll], classOf[apoc.map.Maps], classOf[Json],
      classOf[Create], classOf[apoc.date.Date], classOf[apoc.lock.Lock], classOf[LoadJson], classOf[Xml],
      classOf[PathExplorer], classOf[Meta], classOf[GraphRefactoring], classOf[apoc.help.Help])
    apocProcedures.forEach(proc => {
      try {
        procedures.registerProcedure(proc)
      } catch {
        case e: KernelException => new RuntimeException("Error registering " + proc, e)
      }
    })
    // Init DB
    graphDb.executeTransactionally(initDbQuery)
  }

  override def afterAll(): Unit = {
    dbManagement.shutdown()
    val directory = new Directory(new File(DATABASE_NAME))
    directory.deleteRecursively()
    cdmia.core.categorytheory.Config.disableRequire = false
    cdmia.datawrapper.Config.disableRequire = false
  }

  test("should build a schema") {

    val driver: Driver = GraphDatabase.driver("bolt://localhost:7687", AuthTokens.none)

    val source = new PropertyGraphNeo4jSource(driver)
    val schema = source.getPropertyGraphSchema(DATABASE_NAME)

    // Check objects
    val expectedObjectNames = List[String](":Actor:Person", ":Movie", "ACTED_IN", "label", "string", "number")
    val obtainedObjects: Map[String, Object] = (for (obj <- schema.category.objects) yield obj.name -> obj).toMap
    checkObjects(expectedObjectNames, obtainedObjects)

    // Check morphisms
    val expectedMorphisms = List[ExpectedMorphism](
      ExpectedMorphism("name", obtainedObjects(":Actor:Person"), obtainedObjects("string")),
      ExpectedMorphism("born", obtainedObjects(":Actor:Person"), obtainedObjects("number")),
      ExpectedMorphism("title", obtainedObjects(":Movie"), obtainedObjects("string")),
      ExpectedMorphism("released", obtainedObjects(":Movie"), obtainedObjects("number")),
      ExpectedMorphism("tagline", obtainedObjects(":Movie"), obtainedObjects("string")),
      ExpectedMorphism("roles", obtainedObjects("ACTED_IN"), obtainedObjects("string")),
      ExpectedMorphism("Person", obtainedObjects(":Actor:Person"), obtainedObjects("label")),
      ExpectedMorphism("Actor", obtainedObjects(":Actor:Person"), obtainedObjects("label")),
      ExpectedMorphism("Movie", obtainedObjects(":Movie"), obtainedObjects("label")),
      ExpectedMorphism("ACTED_IN", obtainedObjects("ACTED_IN"), obtainedObjects("label")),
      ExpectedMorphism("ACTED_IN_in", obtainedObjects("ACTED_IN"), obtainedObjects(":Movie")),
      ExpectedMorphism("ACTED_IN_out", obtainedObjects("ACTED_IN"), obtainedObjects(":Actor:Person"))
    )
    val obtainedMorphisms: Map[String, Morphism] = (for (morphism <- schema.category.morphisms) yield morphism.name -> morphism).toMap
    checkMorphisms(expectedMorphisms, obtainedMorphisms)

    // Check object transformations
    val expectedObjectTransformations = Map[Object, Object](
      obtainedObjects(":Actor:Person") -> PropertyGraphModel.vertex,
      obtainedObjects(":Movie") -> PropertyGraphModel.vertex,
      obtainedObjects("ACTED_IN") -> PropertyGraphModel.edge,
      obtainedObjects("label") -> PropertyGraphModel.label,
      obtainedObjects("string") -> PropertyGraphModel.string,
      obtainedObjects("number") -> PropertyGraphModel.number
    )
    checkObjectTransformations(schema.functorTowardsModel, expectedObjectTransformations)

    // Check morphism transformations
    val expectedMorphismTransformations = Map[Morphism, Morphism](
      obtainedMorphisms("name") -> (PropertyGraphModel.stringType o PropertyGraphModel.vertexAttribute),
      obtainedMorphisms("born") -> (PropertyGraphModel.numberType o PropertyGraphModel.vertexAttribute),
      obtainedMorphisms("title") -> (PropertyGraphModel.stringType o PropertyGraphModel.vertexAttribute),
      obtainedMorphisms("released") -> (PropertyGraphModel.numberType o PropertyGraphModel.vertexAttribute),
      obtainedMorphisms("tagline") -> (PropertyGraphModel.stringType o PropertyGraphModel.vertexAttribute),
      obtainedMorphisms("roles") -> (PropertyGraphModel.stringType o PropertyGraphModel.edgeAttribute),
      obtainedMorphisms("Person") -> PropertyGraphModel.vertexLabel,
      obtainedMorphisms("Actor") -> PropertyGraphModel.vertexLabel,
      obtainedMorphisms("Movie") -> PropertyGraphModel.vertexLabel,
      obtainedMorphisms("ACTED_IN") -> PropertyGraphModel.edgeLabel,
      obtainedMorphisms("ACTED_IN_in") -> PropertyGraphModel.in,
      obtainedMorphisms("ACTED_IN_out") -> PropertyGraphModel.out
    )
    checkMorphismTransformations(schema.functorTowardsModel, expectedMorphismTransformations)
  }

  test("should run in a reasonable time") {
    val driver: Driver = GraphDatabase.driver("bolt://localhost:7687", AuthTokens.none)
    val source = new PropertyGraphNeo4jSource(driver)

    val startTime = System.currentTimeMillis()
    val schema = source.getPropertyGraphSchema(DATABASE_NAME)
    info(s"Run in ${System.currentTimeMillis() - startTime}ms")
  }

  private val initDbQuery =
    """CREATE (Keanu:Person:Actor {name:'Keanu Reeves', born:1964})
      |CREATE (TomH:Person:Actor {name:'Tom Hanks', born:1956})
      |
      |CREATE (TheMatrix:Movie {title:'The Matrix', released:1999, tagline:'Welcome to the Real World'})
      |CREATE (TheMatrixReloaded:Movie {title:'The Matrix Reloaded', released:2003, tagline:'Free your mind'})
      |CREATE (TheMatrixRevolutions:Movie {title:'The Matrix Revolutions', released:2003, tagline:'Everything that has a beginning has an end'})
      |CREATE (SomethingsGottaGive:Movie {title:"Something's Gotta Give", released:2003})
      |CREATE (TheDevilsAdvocate:Movie {title:"The Devil's Advocate", released:1997, tagline:'Evil has its winning ways'})
      |
      |CREATE (YouveGotMail:Movie {title:"You've Got Mail", released:1998, tagline:'At odds in life... in love on-line.'})
      |CREATE (SleeplessInSeattle:Movie {title:'Sleepless in Seattle', released:1993, tagline:'What if someone you never met, someone you never saw, someone you never knew was the only someone for you?'})
      |CREATE (ThatThingYouDo:Movie {title:'That Thing You Do', released:1996, tagline:'In every life there comes a time when that thing you dream becomes that thing you do'})
      |CREATE (CloudAtlas:Movie {title:'Cloud Atlas', released:2012, tagline:'Everything is connected'})
      |
      |CREATE (Keanu)-[:ACTED_IN {roles:['Neo']}]->(TheMatrix)
      |CREATE (Keanu)-[:ACTED_IN {roles:['Neo']}]->(TheMatrixReloaded)
      |CREATE (Keanu)-[:ACTED_IN {roles:['Neo']}]->(TheMatrixRevolutions)
      |CREATE (Keanu)-[:ACTED_IN {roles:['Julian Mercer']}]->(SomethingsGottaGive)
      |CREATE (Keanu)-[:ACTED_IN {roles:['Kevin Lomax']}]->(TheDevilsAdvocate)
      |
      |CREATE (TomH)-[:ACTED_IN {roles:['Joe Fox']}]->(YouveGotMail)
      |CREATE (TomH)-[:ACTED_IN {roles:['Sam Baldwin']}]->(SleeplessInSeattle)
      |CREATE (TomH)-[:ACTED_IN {roles:['Mr. White']}]->(ThatThingYouDo)
      |CREATE (TomH)-[:ACTED_IN {roles:['Zachry', 'Dr. Henry Goose', 'Isaac Sachs', 'Dermot Hoggins']}]->(CloudAtlas)""".stripMargin
}
