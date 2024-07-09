package cdmia.core.categorytheory.pattern.limit

import cdmia.core.categorytheory
import cdmia.core.categorytheory.{Category, Object}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.Morphism
import cdmia.core.categorytheory.pattern.Pattern

import java.lang.Object as JObject

/**
 * Represents a limit with its vertex. Each type of limit must implements this class to define their specificities.
 *
 * @param vertex the [[categorytheory.Object]] that is the vertex of the cone of this limit.
 */
abstract class Limit(val vertex: categorytheory.Object, name: String = "Limit") extends Pattern(name) {
  protected val objectsToCheck: Iterable[categorytheory.Object]
  protected val morphismsToCheck: Map[categorytheory.Object, Morphism]
  
  /**
   * Check if this limit is valid within a given category. The conditions of the validity depends on
   * the type of the limit.
   *
   * @param category the [[Category]] in which to check the validity of this limit.
   * @return true is this limit is valid, false otherwise.
   */
  override def isValid(category: Category): Boolean

  /**
   * Explain why the limit is not valid within a given category. The conditions of the validity depends on
   * the type of the limit.
   *
   * @param category the [[Category]] in which to check the validity of this limit.
   * @return a [[List]] of invalidity reasons.
   */
  override def explainIsNotValid(category: Category): List[String]

  /**
   * Create this limit in the destination category of a functor, according to the specified object and morphism transformations.
   * 
   * @param functor the [[Functor]] specifying the object and morphism transformations.
   * @return the [[Limit]] in the destination category.
   */
  override def createPatternInDestinationCategory(functor: Functor): Limit
  
  /**
   * Check if this limit is preserved by the given functor. This limit must also be valid and must respect the universal 
   * property in the source category to check its preservation.
   * 
   * @param functor the [[Functor]] for which to check the preservation of this limit.
   * @return true if the limit is preserved, false otherwise.
   */
  override def isPreservedByFunctor(functor: Functor): Boolean = {
    if (this.isValid(functor.domain) && this.respectsUniversalProperty(functor.domain)) {
      val limit: Limit = createPatternInDestinationCategory(functor)
      limit.isValid(functor.codomain) && limit.respectsUniversalProperty(functor.codomain)
    } else {
      false
    }
  }

  /**
   * Create this limit in the source category of a functor, according to the specified object and morphism transformations.
   *
   * @param functor the [[Functor]] specifying the object and morphism transformations.
   * @return the list of possible [[Limit]]s in the source category.
   */
  override def createPatternsInSourceCategory(functor: Functor): List[Limit]

  /**
   * Check if this limit also exists in a source category. This limit must also be valid and must respect the universal
   * property in the destination category to check its existence.
   *
   * @param functor the [[Functor]] for which to check the existence of this limit.
   * @return the list of [[Limit]]s that match the given limit in the source category.
   */
  override def existsInSourceCategory(functor: Functor): List[Limit] = {
    if (this.isValid(functor.codomain) && this.respectsUniversalProperty(functor.codomain)) {
      val limits: List[Limit] = createPatternsInSourceCategory(functor)
      limits.filter(limit => limit.isValid(functor.domain) && limit.respectsUniversalProperty(functor.domain))
    } else {
      List[Limit]()
    }
  }

  /**
   * Check if this limit respects the universal property within a given category (i.e., for every object that can be a
   * vertex of the cone of the limit, there must exist a unique morphism towards the vertex of the limit with witch the
   * diagram commutes).
   *
   * @param category the [[Category]] in which to check the respect of the universal property.
   * @return true is the universal property is respected, false otherwise.
   */
  override def respectsUniversalProperty(category: Category): Boolean = {
    var respectsUniversalProperty: Boolean = true

    for (objVertex <- category.objects if objVertex != vertex && respectsUniversalProperty) {
      // Find all the objects that can be vertex of the cone
      var candidate: Boolean = true
      var candidateVertexMorphisms = Map[categorytheory.Object, List[Morphism]]()
      for (obj <- objectsToCheck if candidate) {
        val objMorphisms = category.getMorphisms(objVertex, obj)
        if (objMorphisms.isEmpty) {
          // The object cannot be vertex: it does not reach some objects of the cone
          candidate = false
        } else {
          // Get the morphisms from the candidate vertex
          candidateVertexMorphisms += obj -> objMorphisms
        }
      }
      // If the object can be a vertex of the cone...
      if (candidate) {
        // ...it must have a unique morphism towards the vertex of the limit that make the diagram commutes.
        val fromVertexCandidateToVertex = category.getMorphisms(objVertex, vertex)
        if (fromVertexCandidateToVertex.isEmpty) {
          respectsUniversalProperty = false
        } else {
          // Get all the possible combinations for the cone.
          var numberOfCones: Int = candidateVertexMorphisms.map(_._2.size).product
          val cones = Array.fill[Map[categorytheory.Object, Morphism]](numberOfCones)(Map[categorytheory.Object, Morphism]())
          for ((obj, objMorphisms) <- candidateVertexMorphisms) {
            numberOfCones /= objMorphisms.size
            var index = 0
            for (morphism <- objMorphisms) {
              for (i <- 0 until numberOfCones) {
                cones(index) = cones(index) + (obj -> morphism)
                index += 1
              }
            }
          }
          // Check that there is a unique morphism that makes the diagram commutes.
          for (cone <- cones) {
            val validMorphisms = fromVertexCandidateToVertex.filter(morphism => {
              var valid: Boolean = true
              for ((obj, objMorphism) <- cone if valid) {
                valid &&= category.areEqual(objMorphism, morphismsToCheck(objMorphism.codomain) o morphism)
              }
              valid
            })
            if (validMorphisms.isEmpty) {
              respectsUniversalProperty = false
            } else if (validMorphisms.size == 1) {
              // If there is a morphism from the vertex towards the vertex candidate, an isomorphism must exist between the two
              val fromVertexToVertexCandidate = category.getMorphisms(vertex, objVertex)
              if (fromVertexToVertexCandidate.nonEmpty) {
                respectsUniversalProperty &&= fromVertexToVertexCandidate.exists(m => category.isAnIsomorphism(m, validMorphisms.head))
              }
            } else {
              respectsUniversalProperty &&= false
            }
          }
        }
      }
    }
    respectsUniversalProperty
  }
}
