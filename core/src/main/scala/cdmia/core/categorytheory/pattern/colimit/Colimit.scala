package cdmia.core.categorytheory.pattern.colimit

import cdmia.core.categorytheory.{Category, Object}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.{Morphism, MorphismComposition}
import cdmia.core.categorytheory.pattern.Pattern

import java.lang.Object as JObject

/**
 * Represents a colimit with its vertex. Each type of colimit must implements this class to define their specificities.
 *
 * @param vertex the [[Object]] that is the vertex of the cocone of this colimit.
 */
abstract class Colimit(val vertex: Object, name: String = "Colimit") extends Pattern(name) {
  protected val objectsToCheck: Iterable[Object]
  protected val morphismsToCheck: Map[Object, Morphism]

  /**
   * Check if this colimit is valid within a given category. The conditions of the validity depends on
   * the type of the colimit.
   *
   * @param category the [[Category]] in which to check the validity of this colimit.
   * @return true is this colimit is valid, false otherwise.
   */
  override def isValid(category: Category): Boolean

  /**
   * Explain why the colimit is not valid within a given category. The conditions of the validity depends on
   * the type of the colimit.
   *
   * @param category the [[Category]] in which to check the validity of this colimit.
   * @return a [[List]] of invalidity reasons.
   */
  override def explainIsNotValid(category: Category): List[String]

  /**
   * Create this colimit in the destination category of a functor, according to the specified object and morphism transformations.
   *
   * @param functor the [[Functor]] specifying the object and morphism transformations.
   * @return the [[Colimit]] in the destination category.
   */
  override def createPatternInDestinationCategory(functor: Functor): Colimit

  /**
   * Check if this colimit is preserved by the given functor. This colimit must also be valid and must respect the universal
   * property in the source category to check its preservation.
   *
   * @param functor the [[Functor]] for which to check the preservation of this colimit.
   * @return true if the colimit is preserved, false otherwise.
   */
  override def isPreservedByFunctor(functor: Functor): Boolean = {
    if (this.isValid(functor.domain) && this.respectsUniversalProperty(functor.domain)) {
      val colimit: Colimit = createPatternInDestinationCategory(functor)
      colimit.isValid(functor.codomain) && colimit.respectsUniversalProperty(functor.codomain)
    } else {
      false
    }
  }

  /**
   * Create this colimit in the source category of a functor, according to the specified object and morphism transformations.
   *
   * @param functor the [[Functor]] specifying the object and morphism transformations.
   * @return the list of possible [[Colimit]]s in the source category.
   */
  override def createPatternsInSourceCategory(functor: Functor): List[Colimit]

  /**
   * Check if this colimit also exists in a source category. This colimit must also be valid and must respect the universal
   * property in the destination category to check its existence.
   *
   * @param functor the [[Functor]] for which to check the existence of this colimit.
   * @return the list of [[Colimit]]s that match the given limit in the source category.
   */
  override def existsInSourceCategory(functor: Functor): List[Colimit] = {
    if (this.isValid(functor.codomain) && this.respectsUniversalProperty(functor.codomain)) {
      val colimits: List[Colimit] = createPatternsInSourceCategory(functor)
      colimits.filter(colimit => colimit.isValid(functor.domain) && colimit.respectsUniversalProperty(functor.domain))
    } else {
      List[Colimit]()
    }
  }

  /**
   * Check if this colimit respects the universal property within a given category (i.e., for every object that can be a
   * vertex of the cocone of the colimit, there must exist a unique morphism from the vertex of the colimit with witch the
   * diagram commutes).
   *
   * @param category the [[Category]] in which to check the respect of the universal property.
   * @return true is the universal property is respected, false otherwise.
   */
  override def respectsUniversalProperty(category: Category): Boolean = {
    var respectsUniversalProperty: Boolean = true

    for (objVertex <- category.objects if objVertex != vertex && respectsUniversalProperty) {
      // Find all the objects that can be vertex of the cocone
      var candidate: Boolean = true
      var candidateVertexMorphisms = Map[Object, List[Morphism]]()
      for (obj <- objectsToCheck if candidate) {
        val objMorphisms = category.getMorphisms(obj, objVertex)
        if (objMorphisms.isEmpty) {
          // The object cannot be vertex: it is not linked to some objects of the cocone
          candidate = false
        } else {
          // Get the morphisms towards the candidate vertex
          candidateVertexMorphisms += obj -> objMorphisms
        }
      }
      // If the object can be a vertex of the cocone...
      if (candidate) {
        // ...it must have a unique morphism from the vertex of the colimit that make the diagram commutes.
        val fromVertexToVertexCandidate = category.getMorphisms(vertex, objVertex)
        if (fromVertexToVertexCandidate.isEmpty) {
          respectsUniversalProperty = false
        } else {
          // Get all the possible combinations for the cocone.
          var numberOfCocones: Int = candidateVertexMorphisms.map(_._2.size).product
          val cocones = Array.fill[Map[Object, Morphism]](numberOfCocones)(Map[Object, Morphism]())
          for ((obj, objMorphisms) <- candidateVertexMorphisms) {
            numberOfCocones /= objMorphisms.size
            var index = 0
            for (morphism <- objMorphisms) {
              for (i <- 0 until numberOfCocones) {
                cocones(index) = cocones(index) + (obj -> morphism)
                index += 1
              }
            }
          }
          // Check that there is a unique morphism that makes the diagram commutes.
          for (cocone <- cocones) {
            val validMorphisms = fromVertexToVertexCandidate.filter(morphism => {
              var valid: Boolean = true
              for ((obj, objMorphism) <- cocone if valid) {
                valid &&= category.areEqual(objMorphism, morphism o morphismsToCheck(objMorphism.domain))
              }
              valid
            })
            if (validMorphisms.isEmpty) {
              respectsUniversalProperty = false
            } else if (validMorphisms.size == 1) {
              // If there is a morphism from the vertex towards the vertex candidate, an isomorphism must exist between the two
              val fromVertexToCandidateVertex = category.getMorphisms(objVertex, vertex)
              if (fromVertexToCandidateVertex.nonEmpty) {
                respectsUniversalProperty &&= fromVertexToCandidateVertex.exists(m => category.isAnIsomorphism(m, validMorphisms.head))
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

