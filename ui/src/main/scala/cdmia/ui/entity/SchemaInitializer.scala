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
    val pkUserAttribute = RelationalModel.Attribute("id", user, RelationalModel.NumberType)
    val pkUser = new RelationalModel.PrimaryKey("pk_user", user, List(pkUserAttribute))
    val pkTweetAttribute = RelationalModel.Attribute("id", tweet, RelationalModel.NumberType)
    val pkTweet = RelationalModel.PrimaryKey("pk_tweet", tweet, List(pkTweetAttribute))
    val fkAttribute = RelationalModel.Attribute("user_id", tweet, RelationalModel.NumberType)
    RelationalSchemaBuilder("Relational schema")
      .addTable(user)
      .addTable(tweet)
      .addAttribute(pkUserAttribute)
      .addPrimaryKey(pkUser)
      .addAttribute(RelationalModel.Attribute("name", user, RelationalModel.StringType))
      .addAttribute(pkTweetAttribute)
      .addPrimaryKey(pkTweet)
      .addAttribute(RelationalModel.Attribute("text", tweet, RelationalModel.StringType))
      .addAttribute(fkAttribute)
      .addForeignKey(new RelationalModel.ForeignKey("fk_user_id", tweet, pkUser, Map(fkAttribute -> pkUserAttribute)))
      .build()
  }

  def exampleJSONSchema(): JSONSchema = {
    val tweet = new JSONModel.Document("tweet")
    val user = new JSONModel.Document("user")
    val publishedBy = new JSONModel.NestedDocument(tweet, "published_by", user)
    JSONSchemaBuilder("JSON schema")
      .addDocument(tweet)
      .addDocument(user)
      .addDocument(publishedBy)
      .addAttribute(new JSONModel.Attribute("text", tweet, JSONModel.StringType))
      .addAttribute(new JSONModel.Attribute("username", user, JSONModel.StringType))
      .addAttribute(new JSONModel.Attribute("age", user, JSONModel.NumberType))
      .build()
  }

  def examplePropertyGraphSchema(): PropertyGraphSchema = {
    val userVertex = new PropertyGraphModel.Vertex("user")
    val hashtagVertex = new PropertyGraphModel.Vertex("hashtag")
    val publishesEdge = new PropertyGraphModel.Edge("publishes", hashtagVertex, userVertex)
    val friendWithEdge = new PropertyGraphModel.Edge("friend_with", userVertex, userVertex)
    PropertyGraphSchemaBuilder("Property graph schema")
      .addVertex(userVertex)
      .addVertex(hashtagVertex)
      .addEdge(publishesEdge)
      .addEdge(friendWithEdge)
      .addVertexAttribute(new PropertyGraphModel.VertexAttribute("name", userVertex, PropertyGraphModel.StringType))
      .addVertexAttribute(new PropertyGraphModel.VertexAttribute("registered_since", userVertex, PropertyGraphModel.DateType))
      .addEdgeAttribute(new PropertyGraphModel.EdgeAttribute("since", friendWithEdge, PropertyGraphModel.DateType))
      .addVertexLabel(new PropertyGraphModel.VertexLabel(userVertex, "USER"))
      .addVertexLabel(new PropertyGraphModel.VertexLabel(userVertex, "PERSON"))
      .addVertexLabel(new PropertyGraphModel.VertexLabel(hashtagVertex, "HASHTAG"))
      .addEdgeLabel(new PropertyGraphModel.EdgeLabel(publishesEdge, "PUBLISHES"))
      .addEdgeLabel(new PropertyGraphModel.EdgeLabel(friendWithEdge, "FRIEND_WITH"))
      .build()
  }
}
