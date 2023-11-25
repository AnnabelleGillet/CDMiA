package cdmia.datawrapper.modeltransformation

import cdmia.core.categorytheory.Object
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.{Isomorphism, Morphism, MorphismComposition}
import cdmia.core.categorytheory.pattern.Pattern

/**
 * To implement to verify the need of creation of a constraint in a transformation.
 */
sealed trait Creation(val name: String, val concernedMorphisms: List[Morphism]) {
  val concernedObjects: List[Object] = getAllObjects(concernedMorphisms)

  protected def getAllObjects(morphisms: List[Morphism]): List[Object] = {
    (for (morphism <- morphisms) yield {
      morphism match
        case composition: MorphismComposition => getAllObjects(composition.chainOfMorphisms)
        case m: Morphism => List[Object](m.domain, m.codomain)
    }).flatten.distinct
  }

  /**
   * Check if a functor does not validate a constraint in the destination category.
   *
   * @param functor: the [[Functor]] with which to check the need of creation.
   * @return a list of [[CreationOutput]] each with concerned [[Morphism]]s and [[Object]]s, a [[Boolean]] indicating
   *         if the constraint mist be created and a [[String]] message representing a success or a failure message.
   */
  def checkCreation(functor: Functor): List[CreationOutput]

  /**
   * Indicates if the need for creation must be check (e.g., some morphisms or objects concerned by a creation are
   * in the destination of transformations).
   *
   * @param functor: the [[Functor]] with which to check the need of creation.
   * @return true if the creation must be check, false otherwise.
   */
  def needToCheck(functor: Functor): Boolean
}

/**
 * To use to verify the creation of an isomorphism.
 *
 * @param name the name of the constraint.
 * @param isomorphism the [[Isomorphism]] for which to check the preservation.
 */
class IsomorphismCreation(name: String, isomorphism: Isomorphism)
  extends Creation(name, List[Morphism](isomorphism.morphism, isomorphism.inverse)) {

  override def checkCreation(functor: Functor): List[CreationOutput] = {
    require(functor.codomain.isAnIsomorphism(isomorphism.morphism, isomorphism.inverse), "The isomorphism must exist in the destination category.")

    val sourceMorphisms: List[Morphism] = functor.getSourceMorphisms(isomorphism.morphism) ::: functor.morphismTransformations.filter(_.destination.isInstanceOf[MorphismComposition]).filter(mt => {
      mt.destination.asInstanceOf[MorphismComposition].chainOfMorphisms.contains(isomorphism.morphism)
    }).map(_.source).toList
    val sourceInverses: List[Morphism] = functor.getSourceMorphisms(isomorphism.inverse) ::: functor.morphismTransformations.filter(_.destination.isInstanceOf[MorphismComposition]).filter(mt => {
      mt.destination.asInstanceOf[MorphismComposition].chainOfMorphisms.contains(isomorphism.inverse)
    }).map(_.source).toList

    val morphismsWithoutInverse: List[Morphism] = sourceMorphisms.filter(m => !sourceInverses.exists(i => i.codomain == m.domain && i.domain == m.codomain))
    val inversesWithoutMorphism: List[Morphism] = sourceInverses.filter(i => !sourceMorphisms.exists(m => i.codomain == m.domain && i.domain == m.codomain))

    val outputs = for (morphism <- morphismsWithoutInverse ::: inversesWithoutMorphism) yield CreationOutput(getAllObjects(List(morphism)), List[Morphism](morphism), false, s"$name must be created: $morphism needs an inverse.")
    outputs ::: (for (morphism <- sourceMorphisms.filter(!morphismsWithoutInverse.contains(_)); inverse <- sourceInverses.filter(!inversesWithoutMorphism.contains(_))
         if inverse.codomain == morphism.domain && inverse.domain == morphism.codomain) yield {
      if (functor.domain.isAnIsomorphism(morphism, inverse)) {
        CreationOutput(getAllObjects(List(morphism, inverse)), List(morphism, inverse), true, s"$name does not need creation: the isomorphism exists in the source category.")
      } else {
        CreationOutput(getAllObjects(List(morphism, inverse)), List(morphism, inverse), true, s"$name must be created: the isomorphism does not exist in the source category.")
      }
    })
  }

  override def needToCheck(functor: Functor): Boolean = {
    val sourceMorphisms: List[Morphism] = functor.getSourceMorphisms(isomorphism.morphism)
    val sourceInverses: List[Morphism] = functor.getSourceMorphisms(isomorphism.inverse)

    sourceMorphisms.nonEmpty || sourceInverses.nonEmpty ||
      functor.morphismTransformations.filter(_.destination.isInstanceOf[MorphismComposition]).exists(mt => {
        mt.destination.asInstanceOf[MorphismComposition].chainOfMorphisms.contains(isomorphism.morphism)
      }) ||
      functor.morphismTransformations.filter(_.destination.isInstanceOf[MorphismComposition]).exists(mt => {
        mt.destination.asInstanceOf[MorphismComposition].chainOfMorphisms.contains(isomorphism.inverse)
      })
  }
}

/**
 * To use to verify the creation of an isomorphism.
 *
 * @param name the name of the constraint.
 * @param pattern the [[Pattern]] for which to check the preservation.
 * @param objectsToCheck the [[Object]]s to reach with the transformation to trigger the verification of the creation.
 * @param morphismsToCheck the [[Morphism]]s to reach with the transformation to trigger the verification of the creation.
 */
class PatternCreation(name: String, pattern: Pattern, objectsToCheck: List[Object], morphismsToCheck: List[Morphism])
  extends Creation(name, pattern.getMorphisms) {

  override def checkCreation(functor: Functor): List[CreationOutput] = {
    require(pattern.isValid(functor.codomain), "The pattern must be valid in the destination category.")
    require(pattern.respectsUniversalProperty(functor.codomain), "The pattern must respect the universal property in the destination category.")

    val sourcePatterns = pattern.createPatternsInSourceCategory(functor).filter(p => {
      p.getObjects.filter(_.isInCategory(functor.domain)).map(functor.getDestinationObject).exists(objectsToCheck.contains(_)) ||
        p.getMorphisms.filter(_.isInCategory(functor.domain)).map(functor.getDestinationMorphism).exists(morphismsToCheck.contains(_))
    })

    for (pattern <- sourcePatterns) yield {
      if (pattern.isValid(functor.domain)) {
        if (pattern.respectsUniversalProperty(functor.domain)) {
          CreationOutput(pattern.getObjects, pattern.getMorphisms, true, s"$name does not need creation: the pattern exists in the source category.")
        } else {
          CreationOutput(pattern.getObjects, pattern.getMorphisms, false, s"$name must be created: the pattern does not respect the universal property in the source category.")
        }
      } else {
        CreationOutput(pattern.getObjects, pattern.getMorphisms, false, s"$name must be created: the pattern is not valid in the source category for the following reasons:\n\t${pattern.explainIsNotValid(functor.domain).mkString("\n\t")}")
      }
    }
  }

  override def needToCheck(functor: Functor): Boolean = {
    objectsToCheck.exists(functor.getSourceObjects(_).nonEmpty) ||
      morphismsToCheck.exists(functor.getSourceMorphisms(_).nonEmpty) ||
      functor.morphismTransformations.filter(_.destination.isInstanceOf[MorphismComposition]).exists(mt => {
        mt.destination.asInstanceOf[MorphismComposition].chainOfMorphisms.exists(morphismsToCheck.contains(_))
      })
  }
}

case class CreationOutput(concernedObjects: List[Object], concernedMorphisms: List[Morphism], preserved: Boolean, message: String)