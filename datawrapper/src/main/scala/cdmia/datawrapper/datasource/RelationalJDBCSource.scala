package cdmia.datawrapper.datasource

import cdmia.datawrapper.model.relational.RelationalModel
import cdmia.datawrapper.schema.relational.{RelationalSchema, RelationalSchemaBuilder}

import java.sql.{Connection, SQLException, Types as SQLTypes}

/**
 * Get a schema from a PostgreSQL database.
 *
 * @param connection the PostgreSQL connection.
 */
class RelationalJDBCSource(val schemaName: String, val connection: Connection) {
  private val metadata = connection.getMetaData
  cdmia.core.categorytheory.Config.disableRequire = true
  cdmia.datawrapper.Config.disableRequire = true

  /**
   * Get a [[RelationalSchema]] from this data source.
   *
   * @param schemas the selected schemas.
   * @param tables the selected tables.
   * @return a [[RelationalSchema]] of the selected elements.
   */
  def getRelationalSchema(schemas: Option[List[String]], tables: Option[List[String]]): RelationalSchema = {
    var relationalSchemaBuilder = RelationalSchemaBuilder(schemaName)
    var relationalTables = Map[String, RelationalModel.Table]()
    try {
      val tablesResultSet = metadata.getTables(null, null, "%", Array[String]("TABLE"))
      // Extract selected tables
      while (tablesResultSet.next()) {
        val schema = tablesResultSet.getString("TABLE_SCHEM").trim
        val table = tablesResultSet.getString("TABLE_NAME").trim
        // If the schema and table are among the selected ones
        if ((schemas.isEmpty || schemas.get.contains(schema)) &&
          (tables.isEmpty || tables.get.contains(table))) {
          val tableName = s"$schema.$table"
          val relationalTable = RelationalModel.Table(tableName)
          relationalTables += tableName -> relationalTable
          relationalSchemaBuilder = relationalSchemaBuilder.addTable(relationalTable)
        }
      }
      tablesResultSet.close()

      // Columns
      val attributesOfTables: Map[RelationalModel.Table, Map[String, RelationalModel.Attribute]] =
          extractColumns(schemas, tables, relationalTables)
      for ((_, attributes) <- attributesOfTables; (_, attribute) <- attributes) {
        relationalSchemaBuilder = relationalSchemaBuilder.addAttribute(attribute)
      }

      // Primary keys
      val primaryKeysOfTables: Map[RelationalModel.Table, Map[String, RelationalModel.PrimaryKey]] =
          extractPrimaryKeys(schemas, tables, relationalTables, attributesOfTables)
      for ((_, tablePrimaryKeys) <- primaryKeysOfTables; (_, primaryKey) <- tablePrimaryKeys) {
        relationalSchemaBuilder = relationalSchemaBuilder.addPrimaryKey(primaryKey)
      }

      // Foreign keys
      val foreignKeysOfTables: Map[RelationalModel.Table, Map[String, RelationalModel.ForeignKey]] =
        extractForeignKeys(schemas, tables, relationalTables, attributesOfTables, primaryKeysOfTables)
      for ((_, tableForeignKeys) <- foreignKeysOfTables; (_, foreignKey) <- tableForeignKeys) {
        relationalSchemaBuilder = relationalSchemaBuilder.addForeignKey(foreignKey)
      }
    } catch {
      case e: SQLException => {
        e.printStackTrace()
      }
    }
    relationalSchemaBuilder.build()
  }

  private def sqlTypeToModelType(sqlType: Int): RelationalModel.DataType = {
    sqlType match {
      //case SQLTypes.BIT => ???
      case SQLTypes.BLOB => RelationalModel.StringType
      case SQLTypes.CHAR => RelationalModel.StringType
      case SQLTypes.CLOB => RelationalModel.StringType
      //case SQLTypes.ARRAY => ???
      case SQLTypes.BIGINT => RelationalModel.NumberType
      case SQLTypes.BINARY => RelationalModel.NumberType
      case SQLTypes.BOOLEAN => RelationalModel.BooleanType
      //case SQLTypes.DATALINK => ???
      case SQLTypes.DATE => RelationalModel.DateType
      case SQLTypes.DECIMAL => RelationalModel.NumberType
      //case SQLTypes.DISTINCT => ???
      case SQLTypes.DOUBLE => RelationalModel.NumberType
      case SQLTypes.FLOAT => RelationalModel.NumberType
      case SQLTypes.INTEGER => RelationalModel.NumberType
      //case SQLTypes.JAVA_OBJECT => ???
      case SQLTypes.LONGNVARCHAR => RelationalModel.StringType
      case SQLTypes.LONGVARBINARY => RelationalModel.NumberType
      case SQLTypes.LONGVARCHAR => RelationalModel.StringType
      case SQLTypes.NCHAR => RelationalModel.StringType
      case SQLTypes.NCLOB => RelationalModel.StringType
      //case SQLTypes.NULL => ???
      case SQLTypes.NUMERIC => RelationalModel.NumberType
      case SQLTypes.NVARCHAR => RelationalModel.StringType
      //case SQLTypes.OTHER => ???
      case SQLTypes.REAL => RelationalModel.NumberType
      //case SQLTypes.REF => ???
      //case SQLTypes.REF_CURSOR => ???
      //case SQLTypes.ROWID => ???
      case SQLTypes.SMALLINT => RelationalModel.NumberType
      case SQLTypes.SQLXML => RelationalModel.StringType
      //case SQLTypes.STRUCT => ???
      case SQLTypes.TIME => RelationalModel.DateType
      case SQLTypes.TIME_WITH_TIMEZONE => RelationalModel.DateType
      case SQLTypes.TIMESTAMP => RelationalModel.DateType
      case SQLTypes.TIMESTAMP_WITH_TIMEZONE => RelationalModel.DateType
      case SQLTypes.TINYINT => RelationalModel.NumberType
      case SQLTypes.VARBINARY => RelationalModel.NumberType
      case SQLTypes.VARCHAR => RelationalModel.StringType
      case _ => RelationalModel.StringType
    }
  }

  private def extractColumns(schemas: Option[List[String]], tables: Option[List[String]],
                             relationalTables: Map[String, RelationalModel.Table]): Map[RelationalModel.Table, Map[String, RelationalModel.Attribute]] = {
    val columns = scala.collection.mutable.Map[RelationalModel.Table, Map[String, RelationalModel.Attribute]]()
    for (table <- relationalTables.values) {
      columns += table -> Map[String, RelationalModel.Attribute]()
    }
    val schemasToRetrieve = schemas.getOrElse(List[String](null))
    for (schema <- schemasToRetrieve) {
      val columnsResultSet = metadata.getColumns(null, if (schema != null) schema.replace("_", "\\_") else schema, null, null)
      while (columnsResultSet.next()) {
        val tableSchema = columnsResultSet.getString("TABLE_SCHEM").trim
        val tableName = columnsResultSet.getString("TABLE_NAME").trim
        if ((schemas.isEmpty || (schemas.isDefined && schemas.get.contains(tableSchema))) &&
          (tables.isEmpty || (tables.isDefined && tables.get.contains(tableName)))) {
          val columnName = columnsResultSet.getString("COLUMN_NAME")
          val columnType = columnsResultSet.getInt("DATA_TYPE")
          val attribute = RelationalModel.Attribute(columnName, relationalTables(s"$tableSchema.$tableName"), sqlTypeToModelType(columnType))
          columns(relationalTables(s"$tableSchema.$tableName")) += columnName -> attribute
        }
      }
      columnsResultSet.close()
    }
    columns.toMap
  }

  private def extractPrimaryKeys(schemas: Option[List[String]], tables: Option[List[String]],
                                 relationalTables: Map[String, RelationalModel.Table],
                                 attributesOfTables: Map[RelationalModel.Table, Map[String, RelationalModel.Attribute]]):
  Map[RelationalModel.Table, Map[String, RelationalModel.PrimaryKey]] = {
    val primaryKeys = scala.collection.mutable.Map[RelationalModel.Table, scala.collection.mutable.Map[String, List[RelationalModel.Attribute]]]()
    for (table <- relationalTables.values) {
      primaryKeys += table -> scala.collection.mutable.Map[String, List[RelationalModel.Attribute]]()
    }
    val schemasToRetrieve = schemas.getOrElse(List[String](null))
    for (schema <- schemasToRetrieve) {
      val pksResultSet = metadata.getPrimaryKeys(null, schema, null)
      while (pksResultSet.next()) {
        val tableSchema = pksResultSet.getString("TABLE_SCHEM").trim
        val tableName = pksResultSet.getString("TABLE_NAME").trim
        if ((schemas.isEmpty || (schemas.isDefined && schemas.get.contains(tableSchema))) &&
          (tables.isEmpty || (tables.isDefined && tables.get.contains(tableName)))) {
          val relationalTable = relationalTables(s"$tableSchema.$tableName")
          val pk = pksResultSet.getString("PK_NAME")
          val pkAttribute = pksResultSet.getString("COLUMN_NAME")
          val relationalAttribute = attributesOfTables(relationalTable)(pkAttribute)
          val pkIndex = pksResultSet.getInt("KEY_SEQ")
          if (pk != null && primaryKeys(relationalTable).contains(pk)) {
            primaryKeys(relationalTable)(pk) :+= relationalAttribute
          } else if (pk != null) {
            primaryKeys(relationalTable) += pk -> List[RelationalModel.Attribute](relationalAttribute)
          } else {
            ???
          }
        }
      }
      pksResultSet.close()
    }
    // Produce the relational primary keys
    (for ((relationalTable, tablePrimaryKeys) <- primaryKeys) yield {
      relationalTable -> (for ((pkName, pkAttributes) <- tablePrimaryKeys) yield {
        pkName -> new RelationalModel.PrimaryKey(pkName, relationalTable, pkAttributes)
      }).toMap
    }).toMap
  }

  private def extractForeignKeys(schemas: Option[List[String]], tables: Option[List[String]],
                                 relationalTables: Map[String, RelationalModel.Table],
                                 attributesOfTables: Map[RelationalModel.Table, Map[String, RelationalModel.Attribute]],
                                 primaryKeysOfTables: Map[RelationalModel.Table, Map[String, RelationalModel.PrimaryKey]]
                                ): Map[RelationalModel.Table, Map[String, RelationalModel.ForeignKey]] = {
    // Table -> (fk name -> (local attribute -> distant attribute))
    val foreignKeysMapping = scala.collection.mutable.Map[RelationalModel.Table, scala.collection.mutable.Map[String, Map[RelationalModel.Attribute, RelationalModel.Attribute]]]()
    // Table -> (fk name -> primary key)
    val foreignKeysReferencedPrimaryKey = scala.collection.mutable.Map[RelationalModel.Table, Map[String, RelationalModel.PrimaryKey]]()
    for (table <- relationalTables.values) {
      foreignKeysMapping += table -> scala.collection.mutable.Map[String, Map[RelationalModel.Attribute, RelationalModel.Attribute]]()
      foreignKeysReferencedPrimaryKey += table -> Map[String, RelationalModel.PrimaryKey]()
    }
    val schemasToRetrieve = schemas.getOrElse(List[String](null))
    for (schema <- schemasToRetrieve) {
      val fksResultSet = metadata.getImportedKeys(null, schema, null)
      while (fksResultSet.next()) {
        val tableSchema = fksResultSet.getString("FKTABLE_SCHEM").trim
        val tableName = fksResultSet.getString("FKTABLE_NAME").trim
        val pkSchema = fksResultSet.getString("PKTABLE_SCHEM")
        val pkTable = fksResultSet.getString("PKTABLE_NAME")
        if ((schemas.isEmpty || (schemas.isDefined && schemas.get.contains(tableSchema))) &&
          (tables.isEmpty || (tables.isDefined && tables.get.contains(tableName))) &&
          (schemas.isEmpty || (schemas.isDefined && schemas.get.contains(pkSchema))) && // Add the foreign key only if
          (tables.isEmpty || (tables.isDefined && tables.get.contains(pkTable)))) { // the referenced table is selected
          val relationalTable = relationalTables(s"$tableSchema.$tableName")
          val relationalDistantTable = relationalTables(s"$pkSchema.$pkTable")
          val fk = fksResultSet.getString("FK_NAME")
          val pk = fksResultSet.getString("PK_NAME")
          val pkAttribute = fksResultSet.getString("PKCOLUMN_NAME")
          val fkLocalAttribute = fksResultSet.getString("FKCOLUMN_NAME")
          val fkIndex = fksResultSet.getInt("KEY_SEQ")
          val relationalLocalAttribute = attributesOfTables(relationalTable)(fkLocalAttribute)
          val relationalDistantAttribute = attributesOfTables(relationalDistantTable)(pkAttribute)
          if (fk != null && foreignKeysReferencedPrimaryKey(relationalTable).contains(fk)) {
            foreignKeysMapping(relationalTable)(fk) += relationalLocalAttribute -> relationalDistantAttribute
          } else if (fk != null) {
            foreignKeysReferencedPrimaryKey(relationalTable) += fk -> primaryKeysOfTables(relationalDistantTable)(pk)
            foreignKeysMapping(relationalTable) += fk -> Map[RelationalModel.Attribute, RelationalModel.Attribute](relationalLocalAttribute -> relationalDistantAttribute)
          } else {
            ???
          }
        }
      }
      fksResultSet.close()
    }

    (for ((relationalTable, foreignKeys) <- foreignKeysReferencedPrimaryKey) yield {
      relationalTable -> (for ((fkName, primaryKey) <- foreignKeys) yield {
        val mapping = foreignKeysMapping(relationalTable)(fkName)
        fkName -> new RelationalModel.ForeignKey(fkName, relationalTable, primaryKey, mapping)
      })
    }).toMap
  }
}
