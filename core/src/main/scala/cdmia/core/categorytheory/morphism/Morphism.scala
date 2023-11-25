package cdmia.core.categorytheory.morphism

import cdmia.core.categorytheory.{Category, Object}

import scala.annotation.targetName

/**
 * A morphism to be used in a category.
 *
 * @param name: the name of this morphism.
 * @param domain: the source [[Object]] of this morphism.
 * @param codomain: the destination [[Object]] of this morphism.
 */
class Morphism (val name: String, val domain: Object, val codomain: Object) {
  /**
   * Checks if this morphism is in the given category.
   *
   * @param category: the [[Category]] in which the existence of this morphism is verified.
   * @return true if the morphism is in the given category, false otherwise.
   */
  def isInCategory(category: Category): Boolean = {
    category.morphisms.concat(category.identityMorphisms).exists(m => m == this)
  }

  /**
   * Helper method to build a [[MorphismTransformation]] to be used in a functor.
   *
   * @param to: the [[Morphism]] destination of the transformation.
   * @return the [[MorphismTransformation]] from this morphism toward the morphism to.
   */
  @targetName("produceMorphismTransformation")
  def ~>(to: Morphism): MorphismTransformation = new MorphismTransformation(this, to)

  /**
   * Returns true if the morphism can be composed with the given morphism, i.e., if the codomain of this morphism is
   * equal to the domain of the given morphism.
   *
   * @param morphism: the [[Morphism]] with which checking the feasibility of the composition.
   * @return true if the composition is possible, false otherwise.
   */
  def canBeComposedWith(morphism: Morphism): Boolean = this.codomain == morphism.domain

  /**
   * Returns the [[Morphism]] composed of this morphism and the given morphism.
   *
   * @param morphism: the [[Morphism]] with which to perform the composition.
   * @return the [[MorphismComposition]].
   */
  def composeWith(morphism: Morphism): MorphismComposition = {
    require(this.codomain == morphism.domain, s"The codomain of the first morphism must be the same as the domain of the second morphism to be composed, but got ${this.codomain} and ${morphism.domain}.")
    new MorphismComposition(this, morphism)
  }

  /**
   * Returns the [[Morphism]] composed of this morphism and the given morphism.
   *
   * @param morphism : the [[Morphism]] with which to perform the composition.
   * @return the [[MorphismComposition]].
   */
  def o(morphism: Morphism): MorphismComposition =
    morphism.composeWith(this)

  /**
   * Returns the [[Morphism]] composed of this morphism and the given morphism.
   *
   * @param morphism : the [[Morphism]] with which to perform the composition.
   * @return the [[MorphismComposition]].
   */
  def ○(morphism: Morphism): MorphismComposition =
    morphism.composeWith(this)

  /**
   * A morphism is equal to another composition if they have the same chain of morphisms, excluding the identity morphisms
   * that are neutral elements in a composition.
   * If this composition has only a morphism and one or several identity morphisms, it is equal to this morphism.
   *
   * @param obj : the other objet with which testing the equality.
   * @return true if both morphisms are equal, false otherwise.
   */
  override def equals(obj: Any): Boolean = {
    obj match {
      case composition: MorphismComposition => composition == this
      case morphism: Morphism => super.equals(morphism)
      case _ => false
    }
  }

  override def toString: String = s"Morphism($name: $domain -> $codomain)"
}
