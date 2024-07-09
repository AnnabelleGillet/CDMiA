package cdmia.core.categorytheory.morphism

import cdmia.core.categorytheory.Category

/**
 * A composition of morphisms. Each morphism of the composition can itself be a composition.
 * A composition is valid only if the object codomain of the first morphism is the same object as the domain of the
 * second morphism.
 *
 * @param first the first [[Morphism]] of this composition (second ○ first).
 * @param second the second [[Morphism]] of this composition (second ○ first).
 */
class MorphismComposition(val first: Morphism, val second: Morphism) extends Morphism(s"${second.name} ○ ${first.name}", first.domain, second.codomain) {
  require(first.codomain == second.domain, "The codomain of the first morphism must be the same as the domain of the second morphism to be composed.")
  /**
   * Gives all the morphisms of the composition in the correct order.
   */
  val chainOfMorphisms: List[Morphism] = (first match {
    case composition: MorphismComposition => composition.chainOfMorphisms
    case _ => List[Morphism](first)
  }) ::: (second match {
    case composition: MorphismComposition => composition.chainOfMorphisms
    case _ => List[Morphism](second)
  })

  override def isInCategory(category: Category): Boolean = {
    chainOfMorphisms.forall(morphism => category.morphisms.concat(category.identityMorphisms).exists(m => m == morphism))
  }

  /**
   * A composition is equal to another composition if they have the same chain of morphisms, excluding the identity morphisms
   * that are neutral elements in a composition. 
   * If this composition has only a morphism and one or several identity morphisms, it is equal to this morphism.
   * 
   * @param obj the other objet with which testing the equality.
   * @return true if both morphisms are equal, false otherwise.
   */
  override def equals(obj: Any): Boolean = {
    obj match {
      case composition: MorphismComposition => this.chainOfMorphisms.filter(!_.isInstanceOf[IdentityMorphism]) == composition.chainOfMorphisms.filter(!_.isInstanceOf[IdentityMorphism])
      case morphism: Morphism => this.chainOfMorphisms.filter(!_.isInstanceOf[IdentityMorphism]) == List[Morphism](morphism)
      case _ => false
    }
  }
}
