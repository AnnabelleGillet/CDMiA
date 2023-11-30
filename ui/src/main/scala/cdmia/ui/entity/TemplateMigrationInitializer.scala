package cdmia.ui.entity

import cdmia.datawrapper.model.graph.PropertyGraphModel
import cdmia.datawrapper.model.hierarchical.JSONModel
import cdmia.datawrapper.model.relational.RelationalModel
import cdmia.datawrapper.modeltransformation.TemplateTransformation
import cdmia.core.categorytheory.functor.Functor

class TemplateMigrationInitializer {
  jsonToRelational()
  relationalToJSON()
  relationalToPropertyGraph()
  //propertyGraphToRelational()

  def jsonToRelational(): Unit = {
    TemplateMigrationHandler.addTemplate(new TemplateTransformation("JSON_to_Relational", JSONModel, RelationalModel,
      new Functor("JSON_to_Relational", JSONModel.category, RelationalModel.category,
        List(
          JSONModel.document ~> RelationalModel.table,
          JSONModel.typedAttribute ~> RelationalModel.attributeDomain,
          JSONModel.array ~> RelationalModel.table,
          JSONModel.number ~> RelationalModel.number,
          JSONModel.string ~> RelationalModel.string,
          JSONModel.boolean ~> RelationalModel.boolean
        ),
        List(
          JSONModel.jsonObject ~> (RelationalModel.inversePrimaryKey o RelationalModel.localForeignKey),
          JSONModel.attribute ~> RelationalModel.attribute,
          JSONModel.arrayAttribute ~> (RelationalModel.inversePrimaryKey o RelationalModel.localForeignKey),
          JSONModel.arrayContent ~> RelationalModel.table.identityMorphism,
          JSONModel.numberType ~> RelationalModel.numberAttribute,
          JSONModel.stringType ~> RelationalModel.stringAttribute,
          JSONModel.booleanType ~> RelationalModel.booleanAttribute
        )
      )
    ))
  }

  def relationalToJSON(): Unit = {
    TemplateMigrationHandler.addTemplate(new TemplateTransformation("Relational_to_JSON", RelationalModel, JSONModel,
      new Functor("Relational_to_JSON", RelationalModel.category, JSONModel.category,
        List(
          RelationalModel.table ~> JSONModel.document,
          RelationalModel.attributeDomain ~> JSONModel.typedAttribute,
          RelationalModel.primaryKeyDomain ~> JSONModel.document,
          RelationalModel.foreignKey ~> JSONModel.document,
          RelationalModel.boolean ~> JSONModel.boolean,
          RelationalModel.number ~> JSONModel.number,
          RelationalModel.string ~> JSONModel.string,
          RelationalModel.date ~> JSONModel.string
        ),
        List(
          RelationalModel.attribute ~> JSONModel.attribute,
          RelationalModel.primaryKey ~> JSONModel.document.identityMorphism,
          RelationalModel.inversePrimaryKey ~> JSONModel.document.identityMorphism,
          RelationalModel.primaryKeyAttribute ~> JSONModel.attribute,
          RelationalModel.to ~> JSONModel.jsonObject,
          RelationalModel.from ~> JSONModel.document.identityMorphism,
          RelationalModel.localForeignKey ~> JSONModel.jsonObject,
          RelationalModel.localCompositeForeignKey ~> JSONModel.attribute,
          RelationalModel.booleanAttribute ~> JSONModel.booleanType,
          RelationalModel.numberAttribute ~> JSONModel.numberType,
          RelationalModel.stringAttribute ~> JSONModel.stringType,
          RelationalModel.dateAttribute ~> JSONModel.stringType
        )
      )
    ))
  }

  def relationalToPropertyGraph(): Unit = {
    TemplateMigrationHandler.addTemplate(new TemplateTransformation("Relational_to_PropertyGraph", RelationalModel, PropertyGraphModel,
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
          RelationalModel.to ~> PropertyGraphModel.out,
          RelationalModel.from ~> PropertyGraphModel.in,
          RelationalModel.localForeignKey ~> PropertyGraphModel.vertex.identityMorphism,
          RelationalModel.localCompositeForeignKey ~> PropertyGraphModel.vertexAttribute,
          RelationalModel.booleanAttribute ~> PropertyGraphModel.booleanType,
          RelationalModel.numberAttribute ~> PropertyGraphModel.numberType,
          RelationalModel.stringAttribute ~> PropertyGraphModel.stringType,
          RelationalModel.dateAttribute ~> PropertyGraphModel.dateType
        )
      )
    ))
  }

  def propertyGraphToRelational(): Unit = {
    TemplateMigrationHandler.addTemplate(new TemplateTransformation("PropertyGraph_to_Relational", PropertyGraphModel, RelationalModel,
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
      )
    ))
  }
}
