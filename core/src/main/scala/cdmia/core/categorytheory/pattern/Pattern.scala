package cdmia.core.categorytheory.pattern

import cdmia.core.categorytheory.{Category, Object}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.Morphism

abstract class Pattern(val name: String = "Pattern") extends Serializable {
  
  def getObjects: List[Object]

  def getMorphisms: List[Morphism]

  /**
   * Check if this pattern is valid within a given category. The conditions of the validity depends on
   * the type of the pattern.
   *
   * @param category : the [[Category]] in which to check the validity of this pattern.
   * @return true is this pattern is valid, false otherwise.
   */
  def isValid(category: Category): Boolean

  /**
   * Explain why the pattern is not valid within a given category. The conditions of the validity depends on
   * the type of the pattern.
   *
   * @param category : the [[Category]] in which to check the validity of this pattern.
   * @return a [[List]] of invalidity reasons.
   */
  def explainIsNotValid(category: Category): List[String]

  /**
   * Create this pattern in the destination category of a functor, according to the specified object and morphism transformations.
   *
   * @param functor : the [[Functor]] specifying the object and morphism transformations.
   * @return the [[Pattern]] in the destination category.
   */
  def createPatternInDestinationCategory(functor: Functor): Pattern

  /**
   * Check if this pattern is preserved by the given functor.
   *
   * @param functor : the [[Functor]] for which to check the preservation of this pattern.
   * @return true if the pattern is preserved, false otherwise.
   */
  def isPreservedByFunctor(functor: Functor): Boolean

  /**
   * Create this pattern in the source category of a functor, according to the specified object and morphism transformations.
   *
   * @param functor : the [[Functor]] specifying the object and morphism transformations.
   * @return the list of possible [[Pattern]]s in the source category.
   */
  def createPatternsInSourceCategory(functor: Functor): List[Pattern]

  /**
   * Check if this Pattern also exists in a source category.
   *
   * @param functor : the [[Functor]] for which to check the existence of this pattern.
   * @return the list of [[Pattern]]s that match the given pattern in the source category.
   */
  def existsInSourceCategory(functor: Functor): List[Pattern]

  /**
   * Check if this pattern respects the universal property within a given category.
   *
   * @param category : the [[Category]] in which to check the respect of the universal property.
   * @return true is the universal property is respected, false otherwise.
   */
  def respectsUniversalProperty(category: Category): Boolean
}
