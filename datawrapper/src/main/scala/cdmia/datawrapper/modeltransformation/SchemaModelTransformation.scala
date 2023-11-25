package cdmia.datawrapper.modeltransformation

import cdmia.datawrapper.model.Model
import cdmia.datawrapper.schema.Schema
import cdmia.core.categorytheory.functor.Functor

/**
 * Defines a model transformation for a schema.
 *
 * @param source: the source [[Schema]].
 * @param destination: the destination [[Model]].
 * @param functor: the [[Functor]] defining the transformation.
 */
class SchemaModelTransformation(val source: Schema, val destination: Model, val functor: Functor) extends Serializable {
  require(functor.domain == source.category, s"The source of the transformation must be the schema, but got ${source.model.name} for the schema and ${functor.domain.name} for the source of the transformation.")
  require(functor.codomain == destination.category, s"The destination of the transformation must be the model, but got ${destination.name} for the model and ${functor.codomain.name} for the destination of the transformation.")

  var template: Option[TemplateTransformation] = None

  /**
   * Apply a template transformation on the schema.
   *
   * @param source: the source [[Schema]].
   * @param template: the [[TemplateTransformation]] to apply.
   */
  def this(source: Schema, template: TemplateTransformation) = {
    this(source, template.destination, template.functor o source.functorTowardsModel)
    this.template = Some(template)
  }

  val creationOutputs: Map[Creation, List[CreationOutput]] = (for (creation <- destination.creationVerifications if creation.needToCheck(functor)) yield creation -> creation.checkCreation(functor)).toMap
  val preservationOutputs: Map[Preservation, PreservationOutput] = (for (preservation <- source.preservationVerifications) yield preservation -> preservation.checkPreservation(functor)).toMap
}