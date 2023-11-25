package cdmia.core.categorytheory.functor

import cdmia.core.categorytheory
import cdmia.core.categorytheory.{Category, Object, ObjectTransformation}
import cdmia.core.categorytheory.morphism.{IdentityMorphism, Morphism, MorphismComposition, MorphismTransformation}
import cdmia.core.categorytheory.pattern.colimit.Colimit
import cdmia.core.categorytheory.pattern.limit.Limit

/**
 * A functor. A valid functor must respect some constraints:
 * - all the source objects of the object transformations must be in the domain category ;
 * - all the destination objects of the object transformations must be in the codomain category ;
 * - all the source morphisms of the morphism transformations must be in the domain category ;
 * - all the destination morphisms of the morphism transformations must be in the codomain category ;
 * - all objects of the source category must be transformed exactly once ;
 * - all morphisms of the source category must be transformed exactly once ;
 * - the morphism transformation must match the object transformations, i.e, F(x -> y) = F(x) -> F(y).
 *
 * The transformations of the identity morphisms must not be given, as they are automatically deduced from the object
 * transformations, as id_F(x) = F(id_x).
 *
 * @param domain: the source [[Category]] of this functor.
 * @param codomain: the destination [[Category]] of this functor.
 * @param objectTransformations: all the transformations of objects.
 * @param morphismTransformations: the transformations for the morphisms that are not identity morphisms.
 */
class Functor(val name: String,
              val domain: Category,
              val codomain: Category,
              val objectTransformations: Iterable[ObjectTransformation],
              val morphismTransformations: Iterable[MorphismTransformation]
             ) {
  /**
   * The transformations for the identity morphisms.
   */
  val identityMorphismTransformations: Iterable[MorphismTransformation] = objectTransformations.map(_.identityMorphismTransformation)

  require(objectTransformations.map(_.source).forall(_.isInCategory(domain)),
    "All the source objects of the object transformations must be in the domain category.")
  require(objectTransformations.map(_.destination).forall(_.isInCategory(codomain)),
    "All the destination objects of the object transformations must be in the codomain category.")
  require(morphismTransformations.map(_.source).forall(_.isInCategory(domain)),
    "All the source morphisms of the morphism transformations must be in the domain category.")
  require(morphismTransformations.map(_.destination).forall(_.isInCategory(codomain)),
    "All the destination morphisms of the morphism transformations must be in the codomain category.")
  require(objectTransformations.map(_.source).toSet.size == domain.objects.size, // We already checked that all source objects are in the domain category
    "All objects of the source category must be transformed exactly once.")
  require(morphismTransformations.map(_.source).toSet.size == domain.morphisms.size, // We already checked that all source morphisms are in the domain category
    "All morphisms of the source category must be transformed exactly once.")
  private val failedTransformations = morphismTransformations.filterNot(mt => objectTransformations.exists(ot => mt.source.domain == ot.source && mt.destination.domain == ot.destination) &&
    objectTransformations.exists(ot => mt.source.codomain == ot.source && mt.destination.codomain == ot.destination))
  require(failedTransformations.isEmpty,
    s"The morphism transformation must match the object transformations, i.e, F(x -> y) = F(x) -> F(y), but got:\n\t${failedTransformations.map(mt => s"F(x -> y) = ${mt.source} -> ${mt.destination} and F(x) = ${objectTransformations.filter(_.source == mt.source.domain).head.destination}, F(y) = ${objectTransformations.filter(_.source == mt.source.codomain).head.destination}").mkString("\n\t")}.")
  require(!morphismTransformations.map(_.source).exists(_.isInstanceOf[IdentityMorphism]),
    "The identity morphism transformations are automatically computed and should not be given.")

  /**
   * Indicates if this functor respects the functorial law (i.e., F(g o f) = F(g) o F(f) and F(id_x) = id_F(x)).
   */
  val respectFunctorialLaw: Boolean = {
    var respect = true
    for ((morphism, equalities) <- domain.morphismEqualitiesForEachMorphism if respect) {
      respect &&= equalities.forall(equality => codomain.areEqual(getDestinationMorphism(morphism), getDestinationMorphism(equality)))
    }
    for ((morphism, equalities) <- domain.morphismEqualitiesForEachComposition if respect) {
      respect &&= equalities.forall(equality => codomain.areEqual(getDestinationMorphism(morphism), getDestinationMorphism(equality)))
    }
    respect
  }

  /**
   * Returns the object in the destination category to which the given object has been mapped.
   */
  def getDestinationObject(obj: categorytheory.Object): categorytheory.Object = {
    require(domain.objects.exists(_ == obj), s"The object $obj must be in the domain category.")
    objectTransformations.filter(_.source == obj).head.destination
  }

  /**
   * Returns the morphism in the destination category to which the given morphism has been mapped.
   */
  def getDestinationMorphism(morphism: Morphism): Morphism = {
    require(domain.existsMorphism(morphism), s"The morphism $morphism must be in the domain category.")
    morphism match
      case identityMorphism: IdentityMorphism =>
        identityMorphismTransformations.filter(_.source == identityMorphism).head.destination
      case composition: MorphismComposition =>
        getDestinationMorphism(composition.second) o getDestinationMorphism(composition.first)
      case _ =>
        morphismTransformations.filter(_.source == morphism).head.destination
  }

  /**
   * Returns the objects in the source category that are mapped to the given object.
   */
  def getSourceObjects(obj: categorytheory.Object): List[categorytheory.Object] = {
    require(codomain.objects.exists(_ == obj), s"The object $obj must be in the codomain category.")
    objectTransformations.filter(_.destination == obj).map(_.source).toList
  }

  /**
   * Returns the morphisms in the source category that are mapped to the given morphism.
   */
  def getSourceMorphisms(morphism: Morphism): List[Morphism] = {
    require(codomain.existsMorphism(morphism), s"The morphism $morphism must be in the codomain category.")
    val allMorphismTransformations = (identityMorphismTransformations ++ morphismTransformations).toList
    morphism match
      case composition: MorphismComposition =>
        val directMappingToComposition = allMorphismTransformations.filter(_.destination == composition).map(_.source)
        val first = getSourceMorphisms(composition.first)
        val second = getSourceMorphisms(composition.second)
        val indirectMappingToCompositon = for (firstMorphism <- first; secondMorphism <- second) yield secondMorphism o firstMorphism
        directMappingToComposition ::: indirectMappingToCompositon
      case _ =>
        allMorphismTransformations.filter(_.destination == morphism).map(_.source)
  }

  lazy val preservedLimits: Map[Limit, Boolean] = (for (limit <- domain.limits) yield limit -> limit.isPreservedByFunctor(this)).toMap
  lazy val preservedColimits: Map[Colimit, Boolean] = (for (colimit <- domain.colimits) yield colimit -> colimit.isPreservedByFunctor(this)).toMap

  /**
   * Returns the [[Functor]] composition of this functor and the given functor.
   *
   * @param functor : the [[Functor]] with which to perform the composition.
   * @return the resulting [[Functor]].
   */
  def composeWith(functor: Functor): Functor = {
    require(this.codomain == functor.domain, s"The codomain of the first functor must be the same as the domain of the second functor to be composed, but got ${this.codomain} and ${functor.domain}.")
    new Functor(s"${functor.name} ○ ${this.name}", this.domain, functor.codomain,
      this.objectTransformations.map(ot => ot.source ~> functor.getDestinationObject(ot.destination)),
      this.morphismTransformations.map(mt => mt.source ~> functor.getDestinationMorphism(mt.destination)))
  }

  /**
   * Returns the [[Functor]] composition of this functor and the given functor.
   *
   * @param functor : the [[Functor]] with which to perform the composition.
   * @return the resulting [[Functor]].
   */
  def o(functor: Functor): Functor =
    functor.composeWith(this)

  /**
   * Returns the [[Functor]] composition of this functor and the given functor.
   *
   * @param functor : the [[Functor]] with which to perform the composition.
   * @return the resulting [[Functor]].
   */
  def ○(functor: Functor): Functor =
    functor.composeWith(this)

  override def toString: String = s"Functor($name: $domain -> $codomain)"
}

