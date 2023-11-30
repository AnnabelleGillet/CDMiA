package cdmia.ui.entity

import cdmia.datawrapper.model.graph.PropertyGraphModel
import cdmia.datawrapper.model.hierarchical.JSONModel
import cdmia.datawrapper.model.relational.RelationalModel
import cdmia.datawrapper.schema.graph.{PropertyGraphSchema, PropertyGraphSchemaBuilder}
import cdmia.datawrapper.schema.hierarchical.{JSONSchema, JSONSchemaBuilder}
import cdmia.datawrapper.schema.relational.{RelationalSchema, RelationalSchemaBuilder}

class SchemaInitializer {
  SchemaHandler.addSchema(exampleRelationalSchema())
  SchemaHandler.addSchema(exampleJSONSchema())
  SchemaHandler.addSchema(examplePropertyGraphSchema())

  def exampleRelationalSchema(): RelationalSchema = {
    val user = new RelationalModel.Table("user")
    val tweet = new RelationalModel.Table("tweet")
    val pkUser = new RelationalModel.PrimaryKeyAttribute("id", user, RelationalModel.NumberType)
    RelationalSchemaBuilder("Relational schema")
      .addTable(user)
      .addTable(tweet)
      .addPrimaryKeyAttribute(pkUser)
      .addAttribute(new RelationalModel.Attribute("name", user, RelationalModel.StringType))
      .addPrimaryKeyAttribute(new RelationalModel.PrimaryKeyAttribute("id", tweet, RelationalModel.NumberType))
      .addAttribute(new RelationalModel.Attribute("text", tweet, RelationalModel.StringType))
      .addForeignKeyAttribute(new RelationalModel.ForeignKeyAttribute("user_id", tweet, pkUser))
      .build()
  }

  def exampleJSONSchema(): JSONSchema = {
    val library = new JSONModel.Document("library")
    val book = new JSONModel.Document("book")
    val bookArray = new JSONModel.ArrayAttribute("collection", library)
    val author = new JSONModel.NestedDocument("author", book)
    JSONSchemaBuilder("JSON schema")
      .addDocument(library)
      .addDocument(book)
      .addDocument(author)
      .addAttribute(new JSONModel.Attribute("name", library, JSONModel.StringType))
      .addAttribute(new JSONModel.Attribute("title", book, JSONModel.StringType))
      .addAttribute(new JSONModel.Attribute("ISBN", book, JSONModel.NumberType))
      .addAttribute(new JSONModel.Attribute("name", author, JSONModel.StringType))
      .addArray(bookArray)
      .addArrayIndex(new JSONModel.ArrayContentObject(bookArray, book))
      .build()
  }

  def examplePropertyGraphSchema(): PropertyGraphSchema = {
    val userVertex = new PropertyGraphModel.Vertex("user")
    val hashtagVertex = new PropertyGraphModel.Vertex("hashtag")
    val publishesEdge = new PropertyGraphModel.Edge("publishes", hashtagVertex, userVertex)
    val friendsWithEdge = new PropertyGraphModel.Edge("frieds_with", userVertex, userVertex)
    PropertyGraphSchemaBuilder("Property graph schema")
      .addVertex(userVertex)
      .addVertex(hashtagVertex)
      .addEdge(publishesEdge)
      .addEdge(friendsWithEdge)
      .addVertexAttribute(new PropertyGraphModel.VertexAttribute("name", userVertex, PropertyGraphModel.StringType))
      .addVertexAttribute(new PropertyGraphModel.VertexAttribute("registered_since", userVertex, PropertyGraphModel.DateType))
      .addEdgeAttribute(new PropertyGraphModel.EdgeAttribute("since", friendsWithEdge, PropertyGraphModel.DateType))
      .addVertexLabel(new PropertyGraphModel.VertexLabel(userVertex, new PropertyGraphModel.Label("USER")))
      .addVertexLabel(new PropertyGraphModel.VertexLabel(userVertex, new PropertyGraphModel.Label("PERSON")))
      .addVertexLabel(new PropertyGraphModel.VertexLabel(hashtagVertex, new PropertyGraphModel.Label("HASHTAG")))
      .addEdgeLabel(new PropertyGraphModel.EdgeLabel(publishesEdge, new PropertyGraphModel.Label("PUBLISHES")))
      .addEdgeLabel(new PropertyGraphModel.EdgeLabel(friendsWithEdge, new PropertyGraphModel.Label("FRIENDS_WITH")))
      .build()
  }
}
