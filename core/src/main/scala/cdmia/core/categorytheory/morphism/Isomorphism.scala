package cdmia.core.categorytheory.morphism

/**
 * Represents an isomorphism, i.e., an invertible morphism.
 *
 * @param morphism: the [[Morphism]].
 * @param inverse: the inverse of the [[Morphism]].
 */
class Isomorphism(val morphism: Morphism, val inverse: Morphism) {
  require(morphism.domain == inverse.codomain,
    s"The domain of the morphism must be the same object as the codomain of the inverse morphism, but got ${morphism.domain} and ${inverse.codomain}")
  require(morphism.codomain == inverse.domain,
    s"The codomain of the morphism must be the same object as the domain of the inverse morphism, but got ${morphism.codomain} and ${inverse.domain}")
}
