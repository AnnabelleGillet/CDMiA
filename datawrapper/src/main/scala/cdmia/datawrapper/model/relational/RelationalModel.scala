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

  val from: Morphism = new Morphism("from", foreignKey, table)
  val to: Morphism = new Morphism("to", foreignKey, primaryKeyDomain)
  val localForeignKey: Morphism = new Morphism("local_foreign_key", table, primaryKeyDomain)
  val localCompositeForeignKey: Morphism = new Morphism("local_composite_foreign_key", table, attributeDomain)

  override val morphisms: Iterable[Morphism] = List[Morphism](
    attribute, stringAttribute, numberAttribute, booleanAttribute, dateAttribute,
    primaryKey, inversePrimaryKey, primaryKeyAttribute,
    from, to, localForeignKey, localCompositeForeignKey
  )

  /*
  Constraints
   */
  val primaryKeyIsomorphism: Isomorphism = new Isomorphism(primaryKey, inversePrimaryKey)
  val foreignKeyPathEquality: MorphismEquality = new MorphismEquality(List[Morphism](localForeignKey o from, to))
  val compositeForeignKeyPathEquality1: MorphismEquality = new MorphismEquality(List[Morphism](localCompositeForeignKey, primaryKeyAttribute o localForeignKey))
  val compositeForeignKeyPathEquality2: MorphismEquality = new MorphismEquality(List[Morphism](localCompositeForeignKey o from, primaryKeyAttribute o to))
  val foreignKeyPullback: Pullback = new Pullback(foreignKey, table, foreignKey, primaryKeyDomain, from, foreignKey.identityMorphism, localForeignKey, primaryKey o inversePrimaryKey o to, "foreign_key")
  val compositeForeignKeyPullback: Pullback = new Pullback(foreignKey, table, foreignKey, attributeDomain, from, foreignKey.identityMorphism, localCompositeForeignKey, primaryKeyAttribute o primaryKey o inversePrimaryKey o to, "composite_foreign_key")

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
    new PatternCreation("Foreign key", foreignKeyPullback, List[Object](foreignKey), List[Morphism](from, to, localForeignKey)),
    new PatternCreation("Composite foreign key", compositeForeignKeyPullback, List[Object](), List[Morphism](localCompositeForeignKey))
  )

  /*
  Category
   */
  override val category: Category = CategoryBuilder(this.name, objects, morphisms)
    .withIsomorphisms(List[Isomorphism](primaryKeyIsomorphism))
    .withMorphismEqualities(List[MorphismEquality](foreignKeyPathEquality, compositeForeignKeyPathEquality1, compositeForeignKeyPathEquality2))
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
  class Attribute(name: String, val table: Table, val dataType: DataType) extends RelationalModelElement(name) {
    val domain: Object = new Object(s"${name}_domain")
    val attributeMorphism: Morphism = new Morphism(name, table.obj, domain)
    val dataTypeMorphism: Morphism = new Morphism(s"${name}_type", domain, dataType.obj)

    val modelAttributeMorphism: Morphism = getAttributeMorphismOfModel(dataType)

    override val objects: List[Object] = List[Object](domain)
    override val morphisms: List[Morphism] = List[Morphism](attributeMorphism, dataTypeMorphism)

    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](domain ~> RelationalModel.attributeDomain)
    override val morphismTransformations: List[MorphismTransformation] = List[MorphismTransformation](attributeMorphism ~> RelationalModel.attribute, dataTypeMorphism ~> modelAttributeMorphism)
  }

  /**
   * Primary key constraint
   *
   * @param name
   * @param table
   */
  abstract class PrimaryKey(name: String, val table: Table) extends RelationalModelElement(name) {
    val primaryKeyDomain: Object = new Object(s"${name}_primary_key_domain")
    val primaryKeyMorphism: Morphism = new Morphism(name, table.obj, primaryKeyDomain)
    val inversePrimaryKeyMorphism: Morphism = new Morphism(s"${name}_inverse", primaryKeyDomain, table.obj)
    val isomorphism: Isomorphism = new Isomorphism(primaryKeyMorphism, inversePrimaryKeyMorphism)

    override val isomorphisms: List[Isomorphism] = List[Isomorphism](isomorphism)

    override val preservationVerifications: List[Preservation] = List[Preservation](new IsomorphismPreservation(s"Primary key $name", isomorphism))
  }

  /**
   * Simple primary key
   *
   * @param name
   * @param table
   * @param dataType
   */
  class PrimaryKeyAttribute(name: String, table: Table, val dataType: DataType) extends PrimaryKey(name, table) {
    val primaryKeyDataType: Morphism = new Morphism(s"${name}_type", this.primaryKeyDomain, dataType.obj)

    override val objects: List[Object] = List[Object](this.primaryKeyDomain)
    override val morphisms: List[Morphism] = List[Morphism](primaryKeyMorphism, inversePrimaryKeyMorphism, primaryKeyDataType)

    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](this.primaryKeyDomain ~> RelationalModel.primaryKeyDomain)
    override val morphismTransformations: List[MorphismTransformation] = List[MorphismTransformation](primaryKeyMorphism ~> RelationalModel.primaryKey, inversePrimaryKeyMorphism ~> RelationalModel.inversePrimaryKey, primaryKeyDataType ~> (getAttributeMorphismOfModel(dataType) o RelationalModel.primaryKeyAttribute))
  }

  case class IndividualComposedPrimaryKeyAttribute(name: String, dataType: DataType) {
    var domain: Option[Object] = None
    var morphismFromPrimaryKey: Option[Morphism] = None
  }
  /**
   * Composed primary key
   *
   * @param name
   * @param table
   * @param individualAttributes
   */
  class ComposedPrimaryKeyAttribute(name: String, table: Table, val individualAttributes: List[IndividualComposedPrimaryKeyAttribute]) extends PrimaryKey(name, table) {
    require(individualAttributes.size > 1, "There must be at least two attributes in a composed primary key.")

    val individualAttributesOfComposedPrimaryKey: List[(Morphism, Object, Morphism, Morphism)] = for (attribute <- individualAttributes) yield {
        val obj = new Object(s"${attribute.name}_domain")
        val morphism: Morphism = new Morphism(s"${attribute.name}", this.primaryKeyDomain, obj)
        attribute.domain = Some(obj)
        attribute.morphismFromPrimaryKey = Some(morphism)
        (morphism, obj, new Morphism(s"${attribute.name}_type", obj, attribute.dataType.obj), getAttributeMorphismOfModel(attribute.dataType))
      }

    override val objects: List[Object] = this.primaryKeyDomain +: individualAttributesOfComposedPrimaryKey.map(_._2)
    override val morphisms: List[Morphism] = List[Morphism](primaryKeyMorphism, inversePrimaryKeyMorphism) ::: individualAttributesOfComposedPrimaryKey.map(_._1) ::: individualAttributesOfComposedPrimaryKey.map(_._3)

    override val objectTransformations: List[ObjectTransformation] = this.primaryKeyDomain ~> RelationalModel.primaryKeyDomain +: individualAttributesOfComposedPrimaryKey.map(_._2 ~> RelationalModel.attributeDomain)
    override val morphismTransformations: List[MorphismTransformation] = List[MorphismTransformation](primaryKeyMorphism ~> RelationalModel.primaryKey, inversePrimaryKeyMorphism ~> RelationalModel.inversePrimaryKey) :::
        individualAttributesOfComposedPrimaryKey.map(_._1 ~> RelationalModel.primaryKeyAttribute) :::
        individualAttributesOfComposedPrimaryKey.map(a => a._3 ~> a._4)
  }

  /**
   * Foreign key constraint
   *
   * @param name
   * @param table
   * @param primaryKey
   */
  abstract class ForeignKey(name: String, val table: Table, val distantPrimaryKey: PrimaryKey) extends RelationalModelElement(name) {
    val foreignKey: Object = new Object(s"${name}_foreign_key")
    val from: Morphism = new Morphism(s"${name}_from", foreignKey, table.obj)
    val to: Morphism = new Morphism(s"${name}_to", foreignKey, distantPrimaryKey.primaryKeyDomain)
    val localForeignKeyAttribute: Morphism = new Morphism(name, table.obj, distantPrimaryKey.primaryKeyDomain)

    val pathEquality: MorphismEquality = new MorphismEquality(List[Morphism](localForeignKeyAttribute o from, to))
    val pullback: Pullback = new Pullback(foreignKey, table.obj, foreignKey, distantPrimaryKey.primaryKeyDomain, from, foreignKey.identityMorphism, localForeignKeyAttribute, distantPrimaryKey.primaryKeyMorphism o distantPrimaryKey.inversePrimaryKeyMorphism o to, s"${name}_foreign_key")

    override val objects: List[Object] = List[Object](foreignKey)

    override val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation](foreignKey ~> RelationalModel.foreignKey)
  }

  /**
   * Simple foreign key
   *
   * @param name
   * @param table
   * @param distantPrimaryKey
   */
  class ForeignKeyAttribute(name: String, table: Table, distantPrimaryKey: PrimaryKeyAttribute) extends ForeignKey(name, table, distantPrimaryKey) {
    override val morphisms: List[Morphism] = List[Morphism](this.from, this.to, localForeignKeyAttribute)

    override val pathEqualities: List[MorphismEquality] = List[MorphismEquality](pathEquality)
    override val limits: List[Limit] = List[Limit](pullback)

    override val morphismTransformations: List[MorphismTransformation] =
      List[MorphismTransformation](this.from ~> RelationalModel.from, this.to ~> RelationalModel.to, localForeignKeyAttribute ~> RelationalModel.localForeignKey)

    override val preservationVerifications: List[Preservation] = List[Preservation](
      new PatternPreservation(s"Foreign key $name", pullback)
    )
  }

  /**
   * Composed foreign key
   *
   * @param name
   * @param table
   * @param distantPrimaryKey
   * @param localAttributesName
   */
  class ComposedForeignKeyAttribute(name: String, table: Table, distantPrimaryKey: ComposedPrimaryKeyAttribute, localAttributesName: Map[IndividualComposedPrimaryKeyAttribute, String]) extends ForeignKey(name, table, distantPrimaryKey) {
    require(distantPrimaryKey.individualAttributes.forall(a => localAttributesName.keys.exists(_ == a)) &&
      localAttributesName.keys.forall(distantPrimaryKey.individualAttributes.contains(_)),
    "The given individual attributes must be the same than those of the composed primary key.")

    val attributeMorphisms: Map[IndividualComposedPrimaryKeyAttribute, Morphism] = localAttributesName.map((attribute, name) => {
      attribute -> new Morphism(name, table.obj, attribute.domain.get)
    }).toMap

    val attributeEqualities: List[MorphismEquality] = attributeMorphisms.map((attribute, morphism) => new MorphismEquality(List[Morphism](attribute.morphismFromPrimaryKey.get o this.localForeignKeyAttribute, morphism))).toList
    val foreignKeyEqualities: List[MorphismEquality] = attributeMorphisms.map((attribute, morphism) => new MorphismEquality(List[Morphism](attribute.morphismFromPrimaryKey.get o this.to, morphism o this.from))).toList

    val pullbacks: Map[PatternPreservation, Pullback] = attributeMorphisms.map((attribute, morphism) => {
      val individualPullback: Pullback = new Pullback(this.foreignKey, table.obj, this.foreignKey, attribute.domain.get, this.from, this.foreignKey.identityMorphism, morphism, attribute.morphismFromPrimaryKey.get o distantPrimaryKey.primaryKeyMorphism o distantPrimaryKey.inversePrimaryKeyMorphism o this.to, s"${name}_composite_foreign_key_${morphism.name}")
      new PatternPreservation(s"Attribute ${morphism.name} of foreign key $name", individualPullback) -> individualPullback
    }).toMap

    override val morphisms: List[Morphism] = List[Morphism](this.from, this.to, localForeignKeyAttribute) ::: attributeMorphisms.values.toList

    override val pathEqualities: List[MorphismEquality] = List[MorphismEquality](pathEquality) ::: attributeEqualities ::: foreignKeyEqualities
    override val limits: List[Limit] = List[Limit](pullback) ::: pullbacks.values.toList

    override val morphismTransformations: List[MorphismTransformation] =
      List[MorphismTransformation](this.from ~> RelationalModel.from, this.to ~> RelationalModel.to, localForeignKeyAttribute ~> RelationalModel.localForeignKey) :::
        attributeMorphisms.values.map(m => m ~> RelationalModel.localCompositeForeignKey).toList

    override val preservationVerifications: List[Preservation] = List[Preservation](
      new PatternPreservation(name, pullback)
    ) ::: pullbacks.keys.toList
  }
}
