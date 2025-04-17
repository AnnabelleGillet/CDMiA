package cdmia.core.categorytheory

import cdmia.core.categorytheory.morphism.{IdentityMorphism, Isomorphism, Morphism, MorphismComposition, MorphismEquality}
import cdmia.core.categorytheory.pattern.colimit.Colimit
import cdmia.core.categorytheory.pattern.limit.Limit

import scala.collection.parallel.immutable.ParMap
import scala.collection.parallel.CollectionConverters._

/**
 * A category. To be valid, it must respect some constraints:
 * - there is no duplicate object ;
 * - there is no duplicate morphism ;
 * - all the domain of the morphisms are objects of this category ;
 * - all the codomain of the morphisms are objects of this category.
 *
 * The identity morphisms must not be provided as the are automatically computed.
 *
 * @param name the name of this category.
 * @param objects an [[Iterable]] that contains the objects of this category.
 * @param morphisms an [[Iterable]] that contains the morphisms of this category.
 * @param _morphismEqualities an [[Iterable]] that contains the morphism equalities of this category.
 * @param isomorphisms an [[Iterable]] that contains the isomorphisms of this category.
 * @param limits an [[Iterable]] that contains the limits of this category.
 * @param colimits an [[Iterable]] that contains the colimits of this category.
 */
class Category private[core] (val name: String,
                              val objects: Iterable[Object],
                              val morphisms: Iterable[Morphism],
                              val _morphismEqualities: Iterable[MorphismEquality],
                              val isomorphisms: Iterable[Isomorphism],
                              val limits: Iterable[Limit],
                              val colimits: Iterable[Colimit]) {
  if (!Config.disableRequire) {
    require(objects.toSet.size == objects.size, "A category cannot be built with duplicate objects.")
    require(morphisms.toSet.size == morphisms.size, "A category cannot be built with duplicate morphisms.")
    require(morphisms.map(_.domain).forall(d => objects.exists(o => o == d)), "All the domain of the morphisms must be in the category.")
    require(morphisms.map(_.codomain).forall(c => objects.exists(o => o == c)), "All the codomain of the morphisms must be in the category.")
    require(!morphisms.exists(_.isInstanceOf[IdentityMorphism]), "The identity morphisms are automatically computed and should not be added.")
  }

  /**
   * The identity morphisms of this category.
   */
  val identityMorphisms: Iterable[Morphism] = objects.map(_.identityMorphism)

  val morphismEqualities: Map[ObjectsIndex, Iterable[MorphismEquality]] = _morphismEqualities.groupMap(me => ObjectsIndex(me.morphisms.head.domain, me.morphisms.head.codomain))(me => {
    new MorphismEquality(me.morphisms.map {
      case composition: MorphismComposition => if (composition.domain == composition.codomain) removeIsomorphisms(composition) else composition
      case e => e
    })
  })

  case class ObjectsIndex(domain: Object, codomain: Object)
  /**
   * For each morphism, gets all the equal morphisms (including itself).
   */
  private val _morphismEqualitiesForEachMorphism: ParMap[ObjectsIndex, Map[Morphism, Set[Morphism]]] =
    (for ((index, allMorphisms) <- morphisms.groupMap(m => ObjectsIndex(m.domain, m.codomain))(m => m).par) yield {
      index -> {
        for (morphism <- allMorphisms) yield {
          val equalities = morphismEqualities.getOrElse(ObjectsIndex(morphism.domain, morphism.codomain), List[MorphismEquality]()).filter(_.morphisms.exists(m => morphism == m))
          val allEqualMorphisms = equalities.flatMap(_.morphisms).filter(m => m != morphism).toSet + morphism // Allows to add the morphism even if it does not have equalities
          morphism -> allEqualMorphisms
        }
      }.toMap
    })
  /**
   * For each composition, gets all the equal morphisms (including itself).
   */
  private val _morphismEqualitiesForEachComposition: ParMap[ObjectsIndex, List[(MorphismComposition, Set[Morphism])]] =
      (for ((index, allMorphismsEqualities) <- morphismEqualities.par) yield {
        index -> {
          for (morphism <- allMorphismsEqualities.flatMap(_.morphisms).toSet if morphism.isInstanceOf[MorphismComposition]) yield {
            val equalities = morphismEqualities.getOrElse(ObjectsIndex(morphism.domain, morphism.codomain), List[MorphismEquality]()).filter(_.morphisms.exists(m => morphism == m))
            val allEqualMorphisms = equalities.flatMap(_.morphisms).filter(m => m != morphism).toSet + morphism // Allows to add the morphism even if it does not have equalities
            (morphism.asInstanceOf[MorphismComposition], allEqualMorphisms)
          }
        }.toList
      })

  /**
   * For each morphism, gets all the equal morphisms by transitivity (including itself).
   */
  val morphismEqualitiesForEachMorphism: ParMap[ObjectsIndex, Map[Morphism, Set[Morphism]]] = {
    var previousSize: ParMap[Morphism, Int] = _morphismEqualitiesForEachMorphism.flatMap(_._2).map(e => e._1 -> e._2.size)
    var tmpResult = for ((index, value) <- _morphismEqualitiesForEachMorphism) yield {
      index -> {for ((morphism, allMorphisms) <- value) yield {
        morphism -> (for (m <- allMorphisms) yield {
          m match
            case composition: MorphismComposition => _morphismEqualitiesForEachComposition(index).filter(_._1 == composition).head._2
            case _ => _morphismEqualitiesForEachMorphism(index)(m)
        }).flatten.concat(allMorphisms)
      }}
    }
    var change = !previousSize.forall((k, v) => v == tmpResult(ObjectsIndex(k.domain, k.codomain))(k).size)
    previousSize = tmpResult.flatMap(_._2).map(e => e._1 -> e._2.size)
    while (change) {
      tmpResult = for ((index, value) <- tmpResult) yield {
        index -> {
          for ((morphism, allMorphisms) <- value) yield {
            morphism -> (for (m <- allMorphisms) yield {
              m match
                case composition: MorphismComposition => _morphismEqualitiesForEachComposition(index).filter(_._1 == composition).head._2
                case _ => _morphismEqualitiesForEachMorphism(index)(m)
            }).flatten.concat(allMorphisms)
          }
        }
      }
      change = !previousSize.forall((k, v) => v == tmpResult(ObjectsIndex(k.domain, k.codomain))(k).size)
      previousSize = tmpResult.flatMap(_._2).map(e => e._1 -> e._2.size)
    }
    tmpResult
  }

  /**
   * For each composition, gets all the equal morphisms by transitivity (including itself).
   */
  val morphismEqualitiesForEachComposition: ParMap[ObjectsIndex, List[(MorphismComposition, Set[Morphism])]] = {
    var previousSize: ParMap[MorphismComposition, Int] = _morphismEqualitiesForEachComposition.flatMap(_._2).map(e => e._1 -> e._2.size)
    var tmpResult = for ((index, value) <- _morphismEqualitiesForEachComposition) yield {
      index -> {
        for ((morphism, allMorphisms) <- value) yield {
          morphism -> (for (m <- allMorphisms) yield {
            m match
              case composition: MorphismComposition => _morphismEqualitiesForEachComposition(index).filter(_._1 == composition).head._2
              case _ => _morphismEqualitiesForEachMorphism(index)(m)
          }).flatten.concat(allMorphisms)
        }
      }.toMap
    }
    var change = !previousSize.forall((k, v) => v == tmpResult(ObjectsIndex(k.domain, k.codomain))(k).size)
    previousSize = tmpResult.flatMap(_._2).map(e => e._1 -> e._2.size)
    while (change) {
      tmpResult = for ((index, value) <- tmpResult) yield {
        index -> {
          for ((morphism, allMorphisms) <- value) yield {
            morphism -> (for (m <- allMorphisms) yield {
              m match
                case composition: MorphismComposition => _morphismEqualitiesForEachComposition(index).filter(_._1 == composition).head._2
                case _ => _morphismEqualitiesForEachMorphism(index)(m)
            }).flatten.concat(allMorphisms)
          }
        }
      }
      change = !previousSize.forall((k, v) => v == tmpResult(ObjectsIndex(k.domain, k.codomain))(k).size)
      previousSize = tmpResult.flatMap(_._2).map(e => e._1 -> e._2.size)
    }
    tmpResult.map((k, v) => k -> v.toList)
  }

  // Used for caching.
  private var paths: Map[(Object, Object), List[Morphism]] = Map[(Object, Object), List[Morphism]]()
  private val morphismsWithDomain: Map[Object, List[Morphism]] = (for (obj <- objects) yield {
    obj -> morphisms.filter(_.domain == obj).toList
  }).toMap
  private val morphismsWithCodomain: Map[Object, List[Morphism]] = (for (obj <- objects) yield {
    obj -> morphisms.filter(_.codomain == obj).toList
  }).toMap

  if (!Config.disableRequire) {
    require(limits.forall(l => l.isValid(this)), "All the limits must be valid in the category.")
    require(colimits.forall(cl => cl.isValid(this)), "All the colimits must be valid in the category.")
    require(limits.forall(l => l.respectsUniversalProperty(this)), "All the limits must respect the universal property in the category.")
    require(colimits.forall(cl => cl.respectsUniversalProperty(this)), "All the colimits must respect the universal property in the category.")
  }

  /**
   * Returns true if the two morphisms are equal, according to the property of category and the defined equalities.
   */
  def areEqual(_morphism1: Morphism, _morphism2: Morphism): Boolean = {
    // Remove isomorphisms
    val morphism1 = _morphism1 match
      case composition: MorphismComposition =>
        removeIsomorphisms(composition)
      case _ => _morphism1
    val morphism2 = _morphism2 match
      case composition: MorphismComposition =>
        removeIsomorphisms(composition)
      case _ => _morphism2

    if (morphism1.domain == morphism2.domain && morphism1.codomain == morphism2.codomain) {
      if (morphism1 == morphism2) { // If they are the same morphism
        true
      } else {
        val indexMorphism1 = ObjectsIndex(morphism1.domain, morphism1.codomain)
        if (morphismEqualitiesForEachMorphism.contains(indexMorphism1) && morphismEqualitiesForEachMorphism(indexMorphism1).contains(morphism1)) { // If the equality has been defined
          morphismEqualitiesForEachMorphism(indexMorphism1)(morphism1).contains(morphism2)
        } else {
          morphism1 match
            case composition: MorphismComposition => { // If the first morphism is a composition
              val indexComposition = ObjectsIndex(composition.domain, composition.codomain)
              if (morphismEqualitiesForEachComposition.contains(indexComposition) && morphismEqualitiesForEachComposition(indexComposition).exists(_._1 == composition)) {
                morphismEqualitiesForEachComposition(indexComposition).filter(_._1 == composition).head._2.contains(morphism2)
              } else {
                val chainOfMorphismsWithoutIdentities = composition.chainOfMorphisms.filter(!_.isInstanceOf[IdentityMorphism])
                if (chainOfMorphismsWithoutIdentities.size == 1) {
                  val indexChainOfMorphismsWithoutIdentitiesHead = ObjectsIndex(chainOfMorphismsWithoutIdentities.head.domain, chainOfMorphismsWithoutIdentities.head.codomain)
                  if (morphismEqualitiesForEachMorphism.contains(indexChainOfMorphismsWithoutIdentitiesHead) && morphismEqualitiesForEachMorphism(indexChainOfMorphismsWithoutIdentitiesHead).contains(chainOfMorphismsWithoutIdentities.head)) { // If the equality has been defined
                    morphismEqualitiesForEachMorphism(indexChainOfMorphismsWithoutIdentitiesHead)(chainOfMorphismsWithoutIdentities.head).contains(morphism2)
                  } else {
                    false
                  }
                } else {
                  if (chainOfMorphismsWithoutIdentities.isEmpty) {
                    val identity = composition.chainOfMorphisms.head
                    val indexIdentity = ObjectsIndex(identity.domain, identity.codomain)
                    if (morphismEqualitiesForEachMorphism.contains(indexIdentity) && morphismEqualitiesForEachMorphism(indexIdentity).contains(identity)) {
                      identity == morphism2 || morphismEqualitiesForEachMorphism(indexIdentity)(identity).contains(morphism2)
                    } else {
                      identity == morphism2
                    }
                  } else {
                    false
                  }
                }
              }
            }
            case _ => false
        }
      }
    } else {
      false
    }
  }

  /**
   * Returns true if the given morphisms form an isomorphism in this category. 
   * The two morphisms must be in this category.
   * 
   * @param morphism the [[Morphism]]
   * @param inverse the inverse of the [[Morphism]]
   */
  def isAnIsomorphism(morphism: Morphism, inverse: Morphism): Boolean = {
    require(morphism.isInCategory(this) && inverse.isInCategory(this), "The morphisms must be in the category.")
    if (morphism.domain == inverse.codomain && morphism.codomain == inverse.domain) {
      if (morphism == inverse && morphism.isInstanceOf[IdentityMorphism]) {
        true
      } else {
        if (morphism.isInstanceOf[MorphismComposition] && inverse.isInstanceOf[MorphismComposition]) {
          val composition = morphism.asInstanceOf[MorphismComposition]
          val inverseComposition = inverse.asInstanceOf[MorphismComposition]
          var isIsomorphism: Boolean = true
          for (i <- composition.chainOfMorphisms.indices if isIsomorphism) {
            isIsomorphism &&= isAnIsomorphism(composition.chainOfMorphisms(i), inverseComposition.chainOfMorphisms(inverseComposition.chainOfMorphisms.size - 1 - i))
          }
          isIsomorphism
        } else {
          isomorphisms.exists(i => (i.morphism == morphism && i.inverse == inverse) || (i.morphism == inverse && i.inverse == morphism))
        }
      }
    } else {
      false
    }
  }

  /**
   * Get all objects that are domain of this object.
   * 
   * @param obj the object for which to find the domains
   * @return the [[List]] of domain objects
   */
  def getAllDomainObjects(obj: Object): List[Object] = {
    getAllDomainObjects(obj, List[Object](obj))
  }
  
  private def getAllDomainObjects(obj: Object, listedObjects: List[Object]): List[Object] = {
    val newDomains = for (morphism <- morphismsWithCodomain(obj) if !listedObjects.contains(morphism.domain)) yield {
      morphism.domain
    }
    val recursiveDomains = (for (domain <- newDomains) yield {
      getAllDomainObjects(domain, listedObjects ::: newDomains)
    }).flatten
    
    listedObjects ::: newDomains ::: recursiveDomains
  }
  
  /**
   * Returns true if the morphism or the composition exists in this category.
   *
   * @param morphism the [[Morphism]] to search in this category.
   * @return true if the [[Morphism]] is found.
   */
  def existsMorphism(morphism: Morphism): Boolean = {
    morphism match {
      case composition: MorphismComposition => composition.chainOfMorphisms.forall(existsMorphism)
      case morphism: Morphism => morphisms.exists(_ == morphism) || identityMorphisms.exists(_ == morphism)
    }
  }

  /**
   * Returns true if at least one morphism exists between the [[Object]] from and the [[Object]] to, whether it is a
   * composition or not.
   *
   * @param from the domain [[Object]] of the morphism.
   * @param to   the codomain [[Object]] of the morphism.
   * @return true if a morphism exists, false otherwise.
   */
  def existsMorphism(from: Object, to: Object): Boolean = {
    require(objects.exists(_ == from) && objects.exists(_ == to), "The objects must be in the category.")
    getMorphisms(from, to).nonEmpty
  }

  /**
   * Returns the morphisms between the [[Object]] from and the [[Object]] to, whether they are a
   * composition or not (the morphisms are not repeated in the composition).
   *
   * @param from the domain [[Object]] of the morphism.
   * @param to   the codomain [[Object]] of the morphism.
   * @return the list of morphisms.
   */
  def getMorphisms(from: Object, to: Object): List[Morphism] = {
    require(objects.exists(_ == from) && objects.exists(_ == to), "The objects must be in the category.")
    getMorphisms(from, to, List[Object]())
  }

  private def getMorphisms(from: Object, to: Object, testedObjects: List[Object]): List[Morphism] = {
    if (paths.contains((from, to))) { // If paths are already cached
      paths((from, to))
    } else {
      val result = if (to == from) {
        List[Morphism](to.identityMorphism)
      } else {
        //(for (morphism <- morphisms.filter(m => m.domain == from && !testedObjects.contains(m.codomain))) yield {
        (for (morphism <- morphismsWithDomain(from).filter(m => !testedObjects.contains(m.codomain))) yield {
          if (morphism.codomain == to) {
            List[Morphism](morphism)
          } else {
            val validPaths = getMorphisms(morphism.codomain, to, testedObjects :+ morphism.domain)
            for (path <- validPaths) yield {
              path o morphism
            }
          }
        }).flatten.toList
      }
      paths = paths + ((from, to) -> result)
      result
    }
  }

  /**
   * Removes the isomorphisms of a morphism composition.
   *
   * @param morphismComposition the [[MorphismComposition]] from which to remove isomorphisms.
   * @return a [[Morphism]] corresponding to the identity morphism if there were only isomorphisms or corresponding to
   *         the new [[Morphism]] without isomorphisms.
   */
  private def removeIsomorphisms(morphismComposition: MorphismComposition): Morphism = {
    var newChainOfMorphisms = List[Morphism]()
    var previousChainOfMorphisms = morphismComposition.chainOfMorphisms
    var previousSize: Int = previousChainOfMorphisms.size

    while (newChainOfMorphisms.size != previousSize) { // Iterate to remove nested isomorphisms
      previousSize = newChainOfMorphisms.size
      newChainOfMorphisms = List[Morphism]()
      if (previousChainOfMorphisms.size == 2 && isAnIsomorphism(previousChainOfMorphisms.head, previousChainOfMorphisms(1))) {
        newChainOfMorphisms = List[Morphism](previousChainOfMorphisms.head.domain.identityMorphism)
      } else {
        if (previousChainOfMorphisms.size == 1) {
          newChainOfMorphisms = previousChainOfMorphisms
        } else {
          var i: Int = 1
          var previousIsIsomorphism: Boolean = false
          while (i < previousChainOfMorphisms.size) {
            val morphism1 = previousChainOfMorphisms(i - 1)
            val morphism2 = previousChainOfMorphisms(i)
            if (!isAnIsomorphism(morphism1, morphism2)) {
              if (!previousIsIsomorphism) {
                newChainOfMorphisms :+= morphism1
              }
              if (i == previousChainOfMorphisms.size - 1) {
                newChainOfMorphisms :+= morphism2
              }
              previousIsIsomorphism = false
            }  else {
              previousIsIsomorphism = true
            }
            i += 1
          }
        }
      }

      previousChainOfMorphisms = newChainOfMorphisms
    }

    if (newChainOfMorphisms.size == 1) {
      newChainOfMorphisms.head
    } else {
      var newComposition = new MorphismComposition(newChainOfMorphisms.head, newChainOfMorphisms.tail.head)
      for (morphism <- newChainOfMorphisms.tail.tail) {
        newComposition = new MorphismComposition(newComposition, morphism)
      }
      newComposition
    }
  }

  override def toString: String = s"Category($name)"
}

