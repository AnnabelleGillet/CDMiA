package cdmia.datawrapper.modeltransformation

import cdmia.datawrapper.model.Model
import cdmia.core.categorytheory.functor.Functor
import cdmia.datawrapper.model.Model

/**
 * Defines a template transformation between two models, that can be used to migrate the model of a schema.
 * 
 * @param source the source [[Model]].
 * @param destination the destination [[Model]].
 * @param functor the [[Functor]] defining the transformation.
 */
class TemplateTransformation(val name: String, val source: Model, val destination: Model, val functor: Functor) extends Serializable {
  require(functor.domain == source.category, s"The source of the transformation must be the source model, but got ${source.name} for the source model and ${functor.domain.name} for the source of the transformation.")
  require(functor.codomain == destination.category, s"The destination of the transformation must be the destination model, but got ${destination.name} for the destination model and ${functor.codomain.name} for the destination of the transformation.")
  
  val creationOutputs: Map[Creation, List[CreationOutput]] = (for (creation <- destination.creationVerifications if creation.needToCheck(functor)) yield creation -> creation.checkCreation(functor)).toMap
  val preservationOutputs: Map[Preservation, PreservationOutput] = (for (preservation <- source.preservationVerifications) yield preservation -> preservation.checkPreservation(functor)).toMap

  override def toString: String = name
}
