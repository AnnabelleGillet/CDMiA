package cdmia.core.categorytheory

import cdmia.core.categorytheory.morphism.IdentityMorphism

/**
 * An object to be used in a category.
 *
 * @param name: the name of this object.
 */
class Object(val name: String) extends Serializable {
  /**
   * The identity morphism of this object.
   */
  val identityMorphism: IdentityMorphism = new IdentityMorphism(this)

  /**
   * Checks if this object is in the given category.
   *
   * @param category : the [[Category]] in which the existence of this object is verified.
   * @return true if the object is in the given category, false otherwise.
   */
  def isInCategory(category: Category): Boolean = {
    category.objects.exists(o => this == o)
  }

  /**
   * Helper method to build an [[ObjectTransformation]] to be used in a functor.
   *
   * @param to: the [[Object]] destination of the transformation.
   * @return the [[ObjectTransformation]] from this object toward the object to.
   */
  def ~>(to: Object): ObjectTransformation = new ObjectTransformation(this, to)

  override def toString: String = s"Object($name)"
}

