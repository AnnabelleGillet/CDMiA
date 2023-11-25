package cdmia.datawrapper.modeltransformation

import cdmia.core.categorytheory.Object
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.{IdentityMorphism, Isomorphism, Morphism, MorphismComposition}
import cdmia.core.categorytheory.pattern.Pattern

/**
 * To implement to verify the preservation of a constraint in a transformation.
 */
sealed trait Preservation(val name: String, val concernedMorphisms: List[Morphism]) {
  val concernedObjects: List[Object] = getAllObjects(concernedMorphisms)

  private def getAllObjects(morphisms: List[Morphism]): List[Object] = {
    (for (morphism <- morphisms) yield {
      morphism match
        case composition: MorphismComposition => getAllObjects(composition.chainOfMorphisms)
        case m: Morphism => List[Object](m.domain, m.codomain)
    }).flatten.distinct
  }

  /**
   * Check if this functor preserves the constraint.
   *
   * @param functor: the [[Functor]] with which to check the constraint.
   * @return an [[PreservationOutput]] with a [[Boolean]] indicating if the constraint is preserved and a [[String]] message
   *         representing a success or a failure message.
   */
  def checkPreservation(functor: Functor): PreservationOutput
}

/**
 * To use to verify the preservation of an isomorphism.
 *
 * @param name the name of the constraint.
 * @param isomorphism the [[Isomorphism]] for which to check the preservation.
 */
class IsomorphismPreservation(name: String, isomorphism: Isomorphism)
  extends Preservation(name, List[Morphism](isomorphism.morphism, isomorphism.inverse)) {

  override def checkPreservation(functor: Functor): PreservationOutput = {
    require(functor.domain.isAnIsomorphism(isomorphism.morphism, isomorphism.inverse), "The isomorphism must exist in the source category.")

    val isPreserved: Boolean = functor.codomain.isAnIsomorphism(functor.getDestinationMorphism(isomorphism.morphism), functor.getDestinationMorphism(isomorphism.inverse))
    if (isPreserved) {
      if (!isomorphism.morphism.isInstanceOf[IdentityMorphism] && functor.getDestinationMorphism(isomorphism.morphism).isInstanceOf[IdentityMorphism]) {
        PreservationOutput(isPreserved, s"$name valid: the isomorphism is correctly preserved in the destination category but it is collapsed in the object ${functor.getDestinationMorphism(isomorphism.morphism).domain}: $name becomes an internal mechanism.")
      } else {
        PreservationOutput(isPreserved, s"$name valid: the isomorphism is correctly preserved in the destination category.")
      }
    } else {
      PreservationOutput(isPreserved, s"$name invalid: the isomorphism is not preserved in the destination category.")
    }
  }
}

/**
 * To use to verify the preservation of a pattern.
 *
 * @param name the name of the constraint.
 * @param pattern the [[Pattern]] for which to check the preservation.
 */
class PatternPreservation(name: String, pattern: Pattern)
  extends Preservation(name, pattern.getMorphisms) {

  override def checkPreservation(functor: Functor): PreservationOutput = {
    require(pattern.isValid(functor.domain), "The pattern must be valid in the source category.")
    require(pattern.respectsUniversalProperty(functor.domain), "The pattern must respect the universal property in the source category.")

    val transformedPattern = pattern.createPatternInDestinationCategory(functor)

    if (transformedPattern.isValid(functor.codomain)) {
      val isPreserved: Boolean = pattern.isPreservedByFunctor(functor)
      if (isPreserved) {
        PreservationOutput(isPreserved, s"$name valid: the pattern is correctly preserved in the destination category.")
      } else {
        PreservationOutput(isPreserved, s"$name invalid: the pattern does not respect the universal property in the destination category.")
      }
    } else {
      PreservationOutput(false, s"$name invalid: the pattern is not valid in the destination category for the following reasons:\n\t${transformedPattern.explainIsNotValid(functor.codomain).mkString("\n\t")}")
    }
  }
}

case class PreservationOutput(preserved: Boolean, message: String)
