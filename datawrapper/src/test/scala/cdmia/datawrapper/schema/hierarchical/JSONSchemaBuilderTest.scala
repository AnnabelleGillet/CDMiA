package cdmia.datawrapper.schema.hierarchical

import cdmia.datawrapper.model.hierarchical.JSONModel._
import org.scalatest.funsuite.*

class JSONSchemaBuilderTest extends AnyFunSuite {
  test("An empty schema can be built") {
    assertResult(true)(JSONSchemaBuilder("test").build().isInstanceOf[JSONSchema])
  }

  test("A schema with a document can be built") {
    val builder = JSONSchemaBuilder("test")
      .addDocument(new Document("d"))
    assertResult(true)(builder.build().isInstanceOf[JSONSchema])
  }

  test("A schema with a nested document can be built") {
    val document = new Document("d")
    val nestedDocument = new Document("nested")
    val builder = JSONSchemaBuilder("test")
      .addDocument(document)
      .addDocument(nestedDocument)
      .addDocument(new NestedDocument(document, "d", nestedDocument))
    assertResult(true)(builder.build().isInstanceOf[JSONSchema])
  }

  test("Adding a nested document without the corresponding document throws an exception") {
    val document = new Document("d")
    val nestedDocument = new Document("nested")
    val builder = JSONSchemaBuilder("test")
      .addDocument(nestedDocument)
    assertThrows[IllegalArgumentException](builder.addDocument(new NestedDocument(document, "d", nestedDocument)))
  }

  test("Adding a nested document without the corresponding nested document throws an exception") {
    val document = new Document("d")
    val nestedDocument = new Document("nested")
    val builder = JSONSchemaBuilder("test")
      .addDocument(document)
    assertThrows[IllegalArgumentException](builder.addDocument(new NestedDocument(document, "d", nestedDocument)))
  }

  test("A schema with a document and simple attributes can be built") {
    val document = new Document("d")
    val builder = JSONSchemaBuilder("test")
      .addDocument(document)
      .addAttribute(new Attribute("a1", document, StringType))
      .addAttribute(new Attribute("a2", document, NumberType))
      .addAttribute(new Attribute("a3", document, BooleanType))
    assertResult(true)(builder.build().isInstanceOf[JSONSchema])
  }

  test("Adding an attribute without the corresponding document throws an exception") {
    val document = new Document("d")
    val builder = JSONSchemaBuilder("test")
      .addDocument(document)
      .addAttribute(new Attribute("a", document, StringType))
    assertThrows[IllegalArgumentException](builder.addAttribute(new Attribute("a", new Document("d2"), StringType)))
  }

  test("A schema with a document and simple array can be built") {
    val document = new Document("d")
    val builder = JSONSchemaBuilder("test")
      .addDocument(document)
      .addArray(new ArrayAttribute("a1", document))
    assertResult(true)(builder.build().isInstanceOf[JSONSchema])
  }

  test("Adding an array without the corresponding document throws an exception") {
    val document = new Document("d")
    val builder = JSONSchemaBuilder("test")
      .addDocument(document)
      .addArray(new ArrayAttribute("a", document))
    assertThrows[IllegalArgumentException](builder.addArray(new ArrayAttribute("a", new Document("d"))))
  }

  test("A schema with a document and an array with type condition can be built") {
    val document = new Document("d")
    val array = new ArrayAttribute("a1", document)
    val builder = JSONSchemaBuilder("test")
      .addDocument(document)
      .addArray(array)
      .addArrayIndex(new AttributeArrayContent(array, StringType))
    assertResult(true)(builder.build().isInstanceOf[JSONSchema])
  }

  test("Adding an index data type array without the corresponding array throws an exception") {
    val document = new Document("d")
    val array = new ArrayAttribute("a1", document)
    val builder = JSONSchemaBuilder("test")
      .addDocument(document)
      .addArray(array)
      .addArrayIndex(new AttributeArrayContent(array, StringType))
    assertThrows[IllegalArgumentException](builder.addArrayIndex(new AttributeArrayContent(new ArrayAttribute("a", document), StringType)))
  }

  test("A schema with a document and an array with object condition can be built") {
    val document = new Document("d")
    val array = new ArrayAttribute("a1", document)
    val builder = JSONSchemaBuilder("test")
      .addDocument(document)
      .addArray(array)
      .addArrayIndex(new DocumentArrayContent(array, document))
    assertResult(true)(builder.build().isInstanceOf[JSONSchema])
  }

  test("Adding an index object array without the corresponding array throws an exception") {
    val document = new Document("d")
    val array = new ArrayAttribute("a1", document)
    val builder = JSONSchemaBuilder("test")
      .addDocument(document)
      .addArray(array)
      .addArrayIndex(new DocumentArrayContent(array, document))
    assertThrows[IllegalArgumentException](builder.addArrayIndex(new DocumentArrayContent(new ArrayAttribute("a", document), document)))
  }
}
