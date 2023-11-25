package cdmia.core.categorytheory

import cdmia.core.categorytheory.morphism.{IdentityMorphism, Isomorphism, Morphism, MorphismEquality}
import cdmia.core.categorytheory.pattern.colimit.Colimit
import cdmia.core.categorytheory.pattern.limit.Limit

/**
 * Helper object to build a category.
 */
class CategoryBuilder private(val name: String,
                              val objects: Iterable[Object],
                              val morphisms: Iterable[Morphism],
                              val morphismEqualities: Iterable[MorphismEquality] = Iterable[MorphismEquality](),
                              val isomorphisms: Iterable[Isomorphism] = Iterable[Isomorphism]()
                             ) {
  /**
   * Add the collection of morphism equalities to this category builder.
   * If a collection of morphism equalities was already set, the new morphism equalities replace the old.
   *
   * @param morphismEqualities: the collection of [[MorphismEquality]] to add.
   * @return the category builder with the morphism equalities added.
   */
  def withMorphismEqualities(morphismEqualities: Iterable[MorphismEquality]): CategoryBuilder = {
    require(morphismEqualities.forall(e => e.morphisms.forall(me => me.isInCategory(this.build()))), "All the morphisms in the equalities must be in the category.")
    new CategoryBuilder(this.name, this.objects, this.morphisms, morphismEqualities, this.isomorphisms)
  }

  /**
   * Add the collection of isomorphisms to this category builder.
   * If a collection of isomorphisms was already set, the new isomorphisms replace the old.
   *
   * @param isomorphisms : the collection of [[Isomorphism]] to add.
   * @return the category builder with the isomorphisms added.
   */
  def withIsomorphisms(isomorphisms: Iterable[Isomorphism]): CategoryBuilder = {
    require(isomorphisms.forall(i => i.morphism.isInCategory(this.build()) && i.inverse.isInCategory(this.build())), "All the morphisms in the isomorphisms must be in the category.")
    new CategoryBuilder(this.name, this.objects, this.morphisms, this.morphismEqualities, isomorphisms)
  }

  /**
   * Create the category according to the parameters given to this category builder.
   *
   * @param limits: an [[Iterable]] containing the limits to add in the category.
   * @param colimits: an [[Iterable]] containing the colimits to add in the category.
   * @return the category.
   */
  def build(limits: Iterable[Limit] = Iterable[Limit](), colimits: Iterable[Colimit] = Iterable[Colimit]()): Category = {
    new Category(this.name, this.objects, this.morphisms, this.morphismEqualities, this.isomorphisms, limits, colimits)
  }
}

/**
 * Helper object to build a category.
 */
object CategoryBuilder  {
  /**
   * To be valid, a category must respect some constraints:
   * - there is no duplicate object ;
   * - there is no duplicate morphism ;
   * - all the domain of the morphisms are objects of this category ;
   * - all the codomain of the morphisms are objects of this category.
   *
   * The identity morphisms must not be provided as the are automatically computed.
   *
   * @param name      : the name of the category being built.
   * @param objects   : an [[Iterable]] that contains the objects of the category being built.
   * @param morphisms : an [[Iterable]] that contains the morphisms of the category being built.
   */
  def apply(name: String, objects: Iterable[Object], morphisms: Iterable[Morphism]): CategoryBuilder = {
    require(objects.toSet.size == objects.size, "A category cannot be built with duplicate objects.")
    require(morphisms.toSet.size == morphisms.size, "A category cannot be built with duplicate morphisms.")
    require(morphisms.map(_.domain).forall(d => objects.exists(o => o == d)), "All the domain of the morphisms must be in the category.")
    require(morphisms.map(_.codomain).forall(c => objects.exists(o => o == c)), "All the codomain of the morphisms must be in the category.")
    require(!morphisms.exists(_.isInstanceOf[IdentityMorphism]), "The identity morphisms are automatically computed and should not be added.")
    new CategoryBuilder(name, objects, morphisms)
  }
}
