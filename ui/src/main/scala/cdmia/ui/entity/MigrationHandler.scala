package cdmia.ui.entity

import cdmia.datawrapper.modeltransformation.SchemaModelTransformation

object MigrationHandler {
  private var migrations: List[SchemaModelTransformation] = List[SchemaModelTransformation]()
  
  def addMigration(migration: SchemaModelTransformation): Unit = {
    migrations :+= migration
  }
  
  def getAllMigrations(): List[SchemaModelTransformation] = {
    migrations
  }
}
