package cdmia.ui.entity

import cdmia.datawrapper.model.Model
import cdmia.datawrapper.model.graph.PropertyGraphModel
import cdmia.datawrapper.model.hierarchical.JSONModel
import cdmia.datawrapper.model.relational.RelationalModel
import cdmia.core.categorytheory.Category

object ModelHandler {
  val relationalModelCategory: Category = RelationalModel.category
  val propertyGraphModelCategory: Category = PropertyGraphModel.category
  val jsonModelCategory: Category = JSONModel.category

  val allCategories: Map[String, Category] = Map[String, Category](
    RelationalModel.name -> RelationalModel.category,
    PropertyGraphModel.name -> PropertyGraphModel.category,
    JSONModel.name -> JSONModel.category
  )

  val allModels: Map[String, Model] = Map[String, Model](
    RelationalModel.name -> RelationalModel,
    PropertyGraphModel.name -> PropertyGraphModel,
    JSONModel.name -> JSONModel
  )
}
