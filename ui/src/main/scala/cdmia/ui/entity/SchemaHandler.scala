package cdmia.ui.entity

import cdmia.datawrapper.model.Model
import cdmia.datawrapper.schema.Schema

object SchemaHandler {
  private var schemas: List[Schema] = List[Schema]()

  def addSchema(schema: Schema): Unit = {
    schemas :+= schema
  }

  def getAllSchemasByModel(): Map[Model, List[Schema]] = {
    schemas.groupBy(_.model)
  }
}
