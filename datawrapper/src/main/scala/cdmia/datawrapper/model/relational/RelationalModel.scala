package cdmia.datawrapper.model.relational

import cdmia.datawrapper.model.Model
import cdmia.datawrapper.modeltransformation.{Creation, IsomorphismCreation, IsomorphismPreservation, PatternCreation, PatternPreservation, Preservation}
import cdmia.core.categorytheory.{Category, CategoryBuilder, Object, ObjectTransformation}
import cdmia.core.categorytheory.morphism.{Isomorphism, Morphism, MorphismEquality, MorphismTransformation}
import cdmia.core.categorytheory.pattern.limit.{Limit, Pullback}

/**
 * Categorical representation of the relational model.
 */
object RelationalModel extends Model("Relational Model") {
  /*
  Objects
   */
  val table: Object = new Object("table")
  val attributeDomain: Object = new Object("attribute_domain")
  val primaryKeyDomain: Object = new Object("primary_key_domain")
  val string: Object = new Object("string")
  val number: Object = new Object("number")
  val boolean: Object = new Object("boolean")
  val date: Object = new Object("date")
  val foreignKey: Object = new Object("foreign_key")

  override val objects: Iterable[Object] = List[Object](
    table, attributeDomain, primaryKeyDomain, string, number, boolean, date, foreignKey
  )

  /*
  Morphisms
   */
  val attribute: Morphism = new Morphism("attribute", table, attributeDomain)
  val stringAttribute: Morphism = new Morphism("string_attribute", attributeDomain, string)
  val numberAttribute: Morphism = new Morphism("number_attribute", attributeDomain, number)
  val booleanAttribute: Morphism = new Morphism("boolean_attribute", attributeDomain, boolean)
  val dateAttribute: Morphism = new Morphism("date_attribute", attributeDomain, date)

  val primaryKey: Morphism = new Morphism("primary_key", table, primaryKeyDomain)
  val inversePrimaryKey: Morphism = new Morphism("inverse_primary_key", primaryKeyDomain, table)
  val primaryKeyAttribute: Morphism = new Morphism("primary_key_attribute", primaryKeyDomain, attributeDomain)
  val primaryKeyAttributePartOfForeignKey: Morphism = new Morphism("primary_key_attribute_part_of_foreign_key", primaryKeyDomain, attributeDomain)

  val from: Morphism = new Morphism("from", foreignKey, table)
  val to: Morphism = new Morphism("to", foreignKey, primaryKeyDomain)
  val foreignKeyConstraint: Morphism = new Morphism("foreign_key_constraint", table, primaryKeyDomain)
  val foreignKeyAttribute: Morphism = new Morphism("foreign_key_attribute", table, attributeDomain)

  override val morphisms: Iterable[Morphism] = List[Morphism](
    attribute, stringAttribute, numberAttribute, booleanAttribute, dateAttribute,
    primaryKey, inversePrimaryKey, primaryKeyAttribute,
    from, to, foreignKeyConstraint, foreignKeyAttribute, primaryKeyAttributePartOfForeignKey
  )

  /*
  Constraints
   */
  val primaryKeyIsomorphism: Isomorphism = new Isomorphism(primaryKey, inversePrimaryKey)
  val foreignKeyPathEquality: MorphismEquality = new MorphismEquality(List[Morphism](foreignKeyConstraint o from, to))
  val primaryKeyAttributePartOfForeignKeyEquality: MorphismEquality = new MorphismEquality(List[Morphism](primaryKeyAttributePartOfForeignKey o primaryKey, primaryKeyAttribute o foreignKeyConstraint))
  val compositeForeignKeyPathEquality1: MorphismEquality = new MorphismEquality(List[Morphism](foreignKeyAttribute, primaryKeyAttribute o foreignKeyConstraint))
  val compositeForeignKeyPathEquality2: MorphismEquality = new MorphismEquality(List[Morphism](foreignKeyAttribute o from, primaryKeyAttribute o to))
  val foreignKeyPullback: Pullback = new Pullback(foreignKey, table, foreignKey, primaryKeyDomain, from, foreignKey.identityMorphism, foreignKeyConstraint, to, "foreign_key")
  val compositeForeignKeyPullback: Pullback = new Pullback(foreignKey, table, foreignKey, attributeDomain, from, foreignKey.identityMorphism, foreignKeyAttribute, primaryKeyAttribute o to, "composite_foreign_key")

  /*
  Preservations
   */
  override val preservationVerifications: List[Preservation] = List[Preservation](
    new IsomorphismPreservation("Primary key", primaryKeyIsomorphism),
    new PatternPreservation("Foreign key", foreignKeyPullback),
    new PatternPreservation("Composite foreign key", compositeForeignKeyPullback)
  )

  /*
  Creations
   */
  override val creationVerifications: List[Creation] = List[Creation](
    new IsomorphismCreation("Primary key", primaryKeyIsomorphism),
    new PatternCreation("Foreign key", foreignKeyPullback, List[Object](foreignKey), List[Morphism](from, to, foreignKeyConstraint)),
    new PatternCreation("Composite foreign key", compositeForeignKeyPullback, List[Object](), List[Morphism](foreignKeyAttribute))
  )

  /*
  Category
   */
  override val category: Category = CategoryBuilder(this.name, objects, morphisms)
    .withIsomorphisms(List[Isomorphism](primaryKeyIsomorphism))
    .withMorphismEqualities(List[MorphismEquality](foreignKeyPathEquality, primaryKeyAttributePartOfForeignKeyEquality, compositeForeignKeyPathEquality1, compositeForeignKeyPathEquality2))
    .build(List[Limit](foreignKeyPullback, compositeForeignKeyPullback))

  /*
  Elements
   */
  sealed trait RelationalModelElement(val name: String) extends Model.Element

  /**
   * Table
   *
   * @param name
   */
  class Table(name: String) extends RelationalModelElement(name) {
    val obj: Object = new Object(name)

    override val objects: List[Object] = List[Object](obj)

    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](obj ~> RelationalModel.table)
  }

  /**
   * DataType
   *
   * @param name
   */
  abstract class DataType(name: String) extends RelationalModelElement(name) {
    val obj: Object = new Object(name)

    override val objects: List[Object] = List[Object](obj)
  }

  object NumberType extends DataType("number") {
    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](obj ~> RelationalModel.number)
  }

  object StringType extends DataType("string") {
    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](obj ~> RelationalModel.string)
  }

  object BooleanType extends DataType("boolean") {
    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](obj ~> RelationalModel.boolean)
  }

  object DateType extends DataType("date") {
    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](obj ~> RelationalModel.date)
  }

  private def getAttributeMorphismOfModel(dataType: DataType): Morphism = {
    dataType match
      case NumberType => RelationalModel.numberAttribute
      case StringType => RelationalModel.stringAttribute
      case BooleanType => RelationalModel.booleanAttribute
      case DateType => RelationalModel.dateAttribute
  }

  /**
   * Attribute
   *
   * @param name
   * @param table
   * @param dataType
   */
  object Attribute {
    def apply(name: String, table: Table, dataType: DataType): Attribute = {
      val _domain = new Object(s"${name}_domain")
      val _dataTypeMorphism = new Morphism(s"${name}_type", _domain, dataType.obj)
      new Attribute(name, table, dataType, _domain, _dataTypeMorphism, List[Object](_domain),
        List[Morphism](_dataTypeMorphism), List[ObjectTransformation](_domain ~> RelationalModel.attributeDomain),
        List[MorphismTransformation](_dataTypeMorphism ~> getAttributeMorphismOfModel(dataType)))
    }
  }

  class Attribute private(name: String, val table: Table, val dataType: DataType,
                          val domain: Object,
                          val dataTypeMorphism: Morphism,
                          override val objects: List[Object],
                          override val morphisms: List[Morphism],
                          override val objectTransformations: List[ObjectTransformation],
                          override val morphismTransformations: List[MorphismTransformation],
                          override val pathEqualities: List[MorphismEquality] = List[MorphismEquality](),
                          override val limits: List[Limit] = List[Limit](),
                          override val preservationVerifications: List[Preservation] = List[Preservation]()
                         ) extends RelationalModelElement(name) {
    val modelAttributeMorphism: Morphism = getAttributeMorphismOfModel(dataType)

    def produceAttributeMorphism(_domain: Object): Morphism = new Morphism(name, table.obj, _domain)

    def producePrimaryKeyAttributes(_domain: Object, primaryKeys: List[PrimaryKey]): Map[PrimaryKey, Morphism] = {
      (for (_primaryKey <- primaryKeys) yield _primaryKey -> new Morphism(s"${name}", _primaryKey.primaryKeyDomain, _domain)).toMap
    }

    def produceFinalAttribute(inPrimaryKeys: List[PrimaryKey], inForeignKeys: List[ForeignKey],
                              attributesMorphism: Map[Attribute, Morphism], primaryKeyAttributesMorphisms: Map[Attribute, Map[PrimaryKey, Morphism]]): Attribute = {
      val _domain = attributesMorphism(this).codomain
      if (inForeignKeys.isEmpty && inPrimaryKeys.isEmpty) {
        // The attribute is neither a foreign or a primary key
        new Attribute(name, table, dataType, _domain, dataTypeMorphism, objects, morphisms :+ attributesMorphism(this),
          objectTransformations, morphismTransformations :+ attributesMorphism(this) ~> RelationalModel.attribute)
      } else {
        // Map each morphism from primary key domain towards attribute domain to its corresponding morphism in the model
        val primaryKeyMorphismTransformations: List[MorphismTransformation] = if (inForeignKeys.isEmpty) {
          inPrimaryKeys.map(primaryKeyAttributesMorphisms(this)(_) ~> RelationalModel.primaryKeyAttribute)
        } else {
          inPrimaryKeys.map(primaryKeyAttributesMorphisms(this)(_) ~> RelationalModel.primaryKeyAttributePartOfForeignKey)
        }

        var foreignKeyMorphisms = List[Morphism]()
        var _morphismEqualitiesOfForeignKeys = List[MorphismEquality]()
        var _pullbacks = List[Limit]()
        var _patternPreservations = List[PatternPreservation]()
        var foreignKeyMorphismTransformations = List[MorphismTransformation]()

        // Handle foreign keys
        if (inPrimaryKeys.isEmpty) {
          for (individualForeignKey <- inForeignKeys) {
            val distantAttribute = individualForeignKey.mapping(this)
            val morphismFromLocalTableToDomain = attributesMorphism(this)
            val morphismFromDistantPkDomainToDomain = primaryKeyAttributesMorphisms(distantAttribute)(individualForeignKey.distantPrimaryKey)
            val morphismFromLocalTableToDistantPkDomain = individualForeignKey.constraintName
            val individualPullback: Pullback = new Pullback(
              individualForeignKey.foreignKey, table.obj, individualForeignKey.foreignKey, _domain,
              individualForeignKey.from, individualForeignKey.foreignKey.identityMorphism, morphismFromLocalTableToDomain, morphismFromDistantPkDomainToDomain o individualForeignKey.to,
              s"${individualForeignKey.name}_composite_foreign_key_${name}"
            )
            val patternPreservation = new PatternPreservation(s"Attribute ${name} of foreign key ${individualForeignKey.name}", individualPullback)

            foreignKeyMorphisms :+= morphismFromLocalTableToDomain
            // Foreign key and individual attribute equality
            _morphismEqualitiesOfForeignKeys :+= MorphismEquality(List(morphismFromLocalTableToDomain, morphismFromDistantPkDomainToDomain o morphismFromLocalTableToDistantPkDomain))
            // Pullback equality
            _morphismEqualitiesOfForeignKeys :+= MorphismEquality(List(morphismFromDistantPkDomainToDomain o individualForeignKey.to, morphismFromLocalTableToDomain o individualForeignKey.from))
            _pullbacks :+= individualPullback
            _patternPreservations :+= patternPreservation
            // Morphism transformation
            foreignKeyMorphismTransformations :+= morphismFromLocalTableToDomain ~> RelationalModel.foreignKeyAttribute
          }
        } else {
          for (individualForeignKey <- inForeignKeys) {
            for (primaryKey <- inPrimaryKeys) {
              val distantAttribute = individualForeignKey.mapping(this)
              val morphismFromLocalPkDomainToDistantDomain = primaryKeyAttributesMorphisms(this)(primaryKey)
              val morphismFromDistantPkDomainToDomain = primaryKeyAttributesMorphisms(distantAttribute)(individualForeignKey.distantPrimaryKey)
              val morphismFromLocalTableToDistantPkDomain = individualForeignKey.constraintName
              val morphismFromLocalTableToDomain = morphismFromLocalPkDomainToDistantDomain o primaryKey.primaryKeyMorphism

              val individualPullback: Pullback = new Pullback(
                individualForeignKey.foreignKey, table.obj, individualForeignKey.foreignKey, _domain,
                individualForeignKey.from, individualForeignKey.foreignKey.identityMorphism, morphismFromLocalTableToDomain, morphismFromDistantPkDomainToDomain o individualForeignKey.to,
                s"${individualForeignKey.name}_composite_foreign_key_${name}"
              )
              val patternPreservation = new PatternPreservation(s"Attribute ${name} of foreign key ${individualForeignKey.name}", individualPullback)
              
              // Foreign key and individual attribute equality
              _morphismEqualitiesOfForeignKeys :+= MorphismEquality(List(morphismFromLocalTableToDomain, morphismFromDistantPkDomainToDomain o morphismFromLocalTableToDistantPkDomain))
              // Pullback equality
              _morphismEqualitiesOfForeignKeys :+= MorphismEquality(List(morphismFromDistantPkDomainToDomain o individualForeignKey.to, morphismFromLocalTableToDomain o individualForeignKey.from))
              _pullbacks :+= individualPullback
              _patternPreservations :+= patternPreservation
            }
          }
        }

        var _morphismEqualities: List[MorphismEquality] = (for (i <- inPrimaryKeys.indices) yield {
          val fromPkDomToAttributeDom1 = primaryKeyAttributesMorphisms(this)(inPrimaryKeys(i))
          for (j <- i + 1 until inPrimaryKeys.size) yield {
            val fromPkDomToAttributeDom2 = primaryKeyAttributesMorphisms(this)(inPrimaryKeys(j))
            MorphismEquality(List(fromPkDomToAttributeDom1 o inPrimaryKeys(i).primaryKeyMorphism, fromPkDomToAttributeDom2 o inPrimaryKeys(j).primaryKeyMorphism))
          }
        }).flatten.toList

        val _dataTypeMorphism = new Morphism(s"${name}_type", attributesMorphism(this).codomain, dataType.obj)
        new Attribute(name, table, dataType, attributesMorphism(this).domain, attributesMorphism(this),
          if (inForeignKeys.isEmpty) List(attributesMorphism(this).codomain) else List[Object](),
          (if (inForeignKeys.isEmpty) List(_dataTypeMorphism) else List[Morphism]()) ::: primaryKeyAttributesMorphisms(this).values.toList ::: foreignKeyMorphisms,
          if (inForeignKeys.isEmpty) List(attributesMorphism(this).codomain ~> RelationalModel.attributeDomain) else List[ObjectTransformation](),
          (if (inForeignKeys.isEmpty) List(_dataTypeMorphism ~> modelAttributeMorphism) else List[MorphismTransformation]()) ::: primaryKeyMorphismTransformations ::: foreignKeyMorphismTransformations,
          pathEqualities ::: _morphismEqualitiesOfForeignKeys, limits ::: _pullbacks, preservationVerifications ::: _patternPreservations
        )
      }
    }

    override def toString: String = s"Relational attribute $name from table ${table.name}"
  }

  /**
   * Primary key constraint
   *
   * @param name the name of the constraint.
   * @param table the table on which the primary key is applied.
   * @param attributes the attributes of the primary key.
   */
  class PrimaryKey(name: String, val table: Table, val attributes: List[Attribute]) extends RelationalModelElement(name) {
    require(attributes.nonEmpty, "There must be at least one attribute in a primary key.")
    require(attributes.map(_.name).distinct.size == attributes.size, s"There must be no duplicate attribute. Given attributes: ${attributes.map(_.name).mkString(", ")}.")
    require(attributes.forall(_.table == table), s"The attributes must all be of the same table ${table.name}. Attributes with a different table: ${attributes.filterNot(_.table == table).map(a => s"${a.name} in ${a.table.name}").mkString(", ")}.")

    // Primary key constraint
    val primaryKeyDomain: Object = new Object(s"${name}_primary_key_domain")
    val primaryKeyMorphism: Morphism = new Morphism(name, table.obj, primaryKeyDomain)
    val inversePrimaryKeyMorphism: Morphism = new Morphism(s"${name}_inverse", primaryKeyDomain, table.obj)
    val isomorphism: Isomorphism = new Isomorphism(primaryKeyMorphism, inversePrimaryKeyMorphism)

    override val objects: List[Object] = List(this.primaryKeyDomain)
    override val morphisms: List[Morphism] = List[Morphism](primaryKeyMorphism, inversePrimaryKeyMorphism)

    override val objectTransformations: List[ObjectTransformation] = List(this.primaryKeyDomain ~> RelationalModel.primaryKeyDomain)
    override val morphismTransformations: List[MorphismTransformation] = List[MorphismTransformation](primaryKeyMorphism ~> RelationalModel.primaryKey, inversePrimaryKeyMorphism ~> RelationalModel.inversePrimaryKey)

    override val isomorphisms: List[Isomorphism] = List[Isomorphism](isomorphism)

    override val preservationVerifications: List[Preservation] = List[Preservation](new IsomorphismPreservation(s"Primary key $name", isomorphism))
  }

  /**
   * Foreign key constraint
   *
   * @param name the name of the constraint.
   * @param table the referencing table of the foreign key.
   * @param distantPrimaryKey the referenced primary key of the distant table.
   * @param mapping the mapping of local attributes to distant attributes of the referenced table.
   */
  class ForeignKey(name: String, val table: Table, val distantPrimaryKey: PrimaryKey, val mapping: Map[Attribute, Attribute]) extends RelationalModelElement(name) {
    require(mapping.keys.forall(_.table == table), s"All the local attributes must be in the table of the foreign key. Not in table: ${mapping.keys.filterNot(_.table == table).mkString(", ")}")
    require(mapping.forall((localAttribute, distantAttribute) => localAttribute.dataType == distantAttribute.dataType),
      s"All the local attributes must have the same type as the mapped distant attribute. Attributes with different types: ${mapping.filterNot((localAttribute, distantAttribute) => localAttribute.dataType == distantAttribute.dataType).mkString(", ")}")
    require(mapping.size == distantPrimaryKey.attributes.size &&
      mapping.values.forall(distantPrimaryKey.attributes.contains(_)) &&
      distantPrimaryKey.attributes.forall(mapping.values.toList.contains(_)),
      s"All the attributes of the primary key must be mapped. Primary key attributes: ${distantPrimaryKey.attributes.map(_.name).mkString(", ")}. Mapped attributes: ${mapping.values.map(_.name).mkString(", ")}."
    )

    val foreignKey: Object = new Object(s"${name}_foreign_key")
    val from: Morphism = new Morphism(s"${name}_from", foreignKey, table.obj)
    val to: Morphism = new Morphism(s"${name}_to", foreignKey, distantPrimaryKey.primaryKeyDomain)
    val constraintName: Morphism = new Morphism(name, table.obj, distantPrimaryKey.primaryKeyDomain)

    val pathEquality: MorphismEquality = new MorphismEquality(List[Morphism](constraintName o from, to))
    val pullback: Pullback = new Pullback(foreignKey, table.obj, foreignKey, distantPrimaryKey.primaryKeyDomain, from, foreignKey.identityMorphism, constraintName, to, s"${name}_foreign_key")

    override val pathEqualities: List[MorphismEquality] = List[MorphismEquality](pathEquality)
    override val limits: List[Limit] = List[Limit](pullback)

    override val objects: List[Object] = List[Object](foreignKey)
    override val morphisms: List[Morphism] = List[Morphism](this.from, this.to, constraintName)

    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](foreignKey ~> RelationalModel.foreignKey)

    override val morphismTransformations: List[MorphismTransformation] =
      List[MorphismTransformation](this.from ~> RelationalModel.from, this.to ~> RelationalModel.to, constraintName ~> RelationalModel.foreignKeyConstraint)

    override val preservationVerifications: List[Preservation] = List[Preservation](
      new PatternPreservation(name, pullback)
    )
  }
}
