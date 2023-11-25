package cdmia.core.categorytheory.morphism

/**
 * An equality among several morphisms of a category. All the morphisms must have the same domain and codomain.
 *
 * @param morphisms: a collection of equal [[Morphism]]s.
 */
class MorphismEquality(val morphisms: Iterable[Morphism]) {
  require(morphisms.toSet.size > 1, "There should be at least two different morphisms.")
  require(morphisms.toSet.size == morphisms.size, "Each morphism should be given only once.")
  require(morphisms.tail.forall(m => morphisms.head.domain == m.domain && morphisms.head.codomain == m.codomain),
  "To be equal, all morphisms must have the same domain and codomain.")
}
