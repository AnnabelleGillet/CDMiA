package cdmia.core.categorytheory

import cdmia.core.categorytheory.morphism.MorphismTransformation

import java.lang

/**
 * An object transformation occurring in a functor.
 * 
 * @param source      the source [[lang.Object]] of this transformation.
 * @param destination the destination [[lang.Object]] of this transformation.
 */
class ObjectTransformation(val source: Object, val destination: Object) {
  /**
   * The identity morphism transformation of the objects of this object transformation. 
   */
  val identityMorphismTransformation: MorphismTransformation = new MorphismTransformation(source.identityMorphism, destination.identityMorphism)

  override def toString: String = s"ObjectTransformation($source ~> $destination)"
}
