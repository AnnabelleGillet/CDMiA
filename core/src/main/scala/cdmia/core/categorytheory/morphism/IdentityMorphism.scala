package cdmia.core.categorytheory.morphism

import cdmia.core.categorytheory.Object

/**
 * The identity morphism of an object.
 *
 * @param obj : the [[Object]] source and destination of the identity morphism.
 */
class IdentityMorphism private[core](val obj: Object) extends Morphism(s"identity_${obj.name}", obj, obj)
