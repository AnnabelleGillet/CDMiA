package cdmia.ui.entity

import cdmia.datawrapper.model.Model
import cdmia.datawrapper.modeltransformation.TemplateTransformation

object TemplateMigrationHandler {
  private var templates: List[TemplateTransformation] = List[TemplateTransformation]()
  
  def addTemplate(template: TemplateTransformation): Unit = {
    templates :+= template
  }
  
  def getAllTemplates(): List[TemplateTransformation] = {
    templates
  }
  
  def getAllTemplatesBySourceModel(): Map[Model, List[TemplateTransformation]] = {
    templates.groupBy(_.source)
  }

  def getAllTemplatesByDestinationModel(): Map[Model, List[TemplateTransformation]] = {
    templates.groupBy(_.destination)
  }
  
  def getTemplates(source: Model, destination: Model): List[TemplateTransformation] = {
    templates.filter(t => t.source == source && t.destination == destination)
  }
}
