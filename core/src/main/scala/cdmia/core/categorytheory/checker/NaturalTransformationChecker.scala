package cdmia.core.categorytheory.checker

import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.Morphism

import java.lang.Object as JObject

/**
 * Check if there is a natural transformation between two functors. To use this class, the two functors
 * must have the same category domain and codomain.
 *
 * @param from the source [[Functor]].
 * @param to the destination [[Functor]]
 */
class NaturalTransformationChecker(from: Functor, to: Functor) {
  require(from.domain == to.domain, s"Both functors must have the same category domain, but got ${from.domain} and ${to.domain}.")
  require(from.codomain == to.codomain, s"Both functors must have the same category codomain, but got ${from.codomain} and ${to.codomain}.")

  private val objects = from.domain.objects
  // Initialize the morphisms candidates for being the component alpha of an object.
  private var alphaCandidates: Map[Object, List[Morphism]] = (for (obj <- objects) yield {
    obj -> from.codomain.getMorphisms(
      from.objectTransformations.find(ot => ot.source == obj).get.destination,
      to.objectTransformations.find(ot => ot.source == obj).get.destination
    )
  }).toMap
  private[core] var alphaSolutions: List[Map[Object, Morphism]] = List[Map[Object, Morphism]]()

  findValidAlphaCandidates()
  val isValid: Boolean = computeGlobalSolutions(Map[Object, Morphism](), objects.head, objects.tail)

  /**
   * From a collection of valid alpha candidates, computes the valid global solutions for the whole category.
   */
  private def computeGlobalSolutions(fixedAlpha: Map[Object, Morphism],
                                     x: Object,
                                     remainingObjects: Iterable[Object]): Boolean = {
    val candidates = alphaCandidates(x)
    val Fx = from.objectTransformations.find(ot => ot.source == x).get.destination
    val Gx = to.objectTransformations.find(ot => ot.source == x).get.destination
    var solutionExists = candidates.nonEmpty
    for (xAlpha <- candidates) {
      var validCandidate = true
      if (fixedAlpha.isEmpty) {
        if (remainingObjects.nonEmpty) { // If it is not the last object, we find an alpha for the next one
          if (!computeGlobalSolutions(fixedAlpha + (x -> xAlpha), remainingObjects.head, remainingObjects.tail)) {
            validCandidate = false
          }
        }
      } else {
        for (f <- from.domain.morphisms.filter(_.domain == x) if validCandidate && fixedAlpha.contains(f.codomain)) { // We check for each morphism with x as domain if the already fixed alpha are compatible
          val y: Object = f.codomain

          val Fy = from.objectTransformations.find(ot => ot.source == y).get.destination
          val Gy = to.objectTransformations.find(ot => ot.source == y).get.destination
          val Ff = from.morphismTransformations.find(mt => mt.source == f).get.destination
          val Gf = to.morphismTransformations.find(mt => mt.source == f).get.destination

          val yAlpha = fixedAlpha(y)

          if (from.codomain.areEqual(yAlpha o Ff, Gf o xAlpha)) {
            if (remainingObjects.nonEmpty) { // If it is not the last object, we find an alpha for the next one
              if (!computeGlobalSolutions(fixedAlpha + (x -> xAlpha), remainingObjects.head, remainingObjects.tail)) {
                validCandidate = false
              }
            }
          } else {
            validCandidate = false
          }
        }

        for (f <- from.domain.morphisms.filter(_.codomain == x) if validCandidate && fixedAlpha.contains(f.domain)) { // We check for each morphism with x as codomain if the already fixed alpha are compatible
          val x2: Object = f.domain
          val y: Object = x // To have the commutativity checking in the usual way
          val x2Alpha = fixedAlpha(x2)

          val Fy = from.objectTransformations.find(ot => ot.source == y).get.destination
          val Gy = to.objectTransformations.find(ot => ot.source == y).get.destination
          val Ff = from.morphismTransformations.find(mt => mt.source == f).get.destination
          val Gf = to.morphismTransformations.find(mt => mt.source == f).get.destination

          val yAlpha = xAlpha // ...we get its alpha candidate...

          if (from.codomain.areEqual(yAlpha o Ff, Gf o x2Alpha)) {
            if (remainingObjects.nonEmpty) { // If it is not the last object, we find an alpha for the next one
              if (!computeGlobalSolutions(fixedAlpha + (x -> xAlpha), remainingObjects.head, remainingObjects.tail)) {
                validCandidate = false
              }
            }
          } else {
            validCandidate = false
          }
        }
      }

      // If a valid solution has been found for al the morphisms related to x with xAlpha, we add the solution to the collection of solutions
      if (validCandidate && remainingObjects.isEmpty) {
        alphaSolutions +:= fixedAlpha + (x -> xAlpha)
      }
      solutionExists = solutionExists && validCandidate
    }
    solutionExists
  }

  /**
   * Updates the alpha candidates by removing those that do not conform to the constraint on morphisms
   * (i.e., alpha_y o F(f) = G(f) o alpha_x)
   */
  private def findValidAlphaCandidates(): Unit = {
    var change: Boolean = true
    while (change) { // While we found a modification
      change = false
      for (x <- alphaCandidates.keys) { // For every object of the domain category...
        val xAlphaCandidates = alphaCandidates(x)
        var xAlphaValidCandidates = for (c <- xAlphaCandidates) yield c
        for (xAlphaCandidate <- xAlphaCandidates) {
          val Fx = from.objectTransformations.find(ot => ot.source == x).get.destination
          val Gx = to.objectTransformations.find(ot => ot.source == x).get.destination
          var candidateValid: Boolean = false
          for (f <- from.domain.morphisms.filter(_.domain == x)) { // ...we get all the morphisms with this object as domain...
            val y: Object = f.codomain // ...and for the codomain of each morphism...

            val Fy = from.objectTransformations.find(ot => ot.source == y).get.destination
            val Gy = to.objectTransformations.find(ot => ot.source == y).get.destination
            val Ff = from.morphismTransformations.find(mt => mt.source == f).get.destination
            val Gf = to.morphismTransformations.find(mt => mt.source == f).get.destination

            val yAlphaCandidates = alphaCandidates(y) // ...we get its alpha candidates...
            val yAlphaValidCandidates = yAlphaCandidates.filter(yAlphaCandidate => {
              from.codomain.areEqual(yAlphaCandidate o Ff, Gf o xAlphaCandidate)
            }) // ...and remove the candidates for which alpha_y o F(f) != G(f) o alpha_x.

            if (yAlphaValidCandidates.isEmpty) { // If we have not found at least one candidate, xAlpha is not a valid candidate.
              xAlphaValidCandidates = xAlphaValidCandidates.filter(_ != xAlphaCandidate)
            }

            if (yAlphaValidCandidates.size != yAlphaCandidates.size) { // If the list of candidates has been updated, we continue the search of valid candidates.
              change = true
              alphaCandidates = (alphaCandidates - y) + (y -> yAlphaValidCandidates)
            }
          }
        }
        if (xAlphaValidCandidates.size != xAlphaCandidates.size) {
          change = true
          alphaCandidates = (alphaCandidates - x) + (x -> xAlphaValidCandidates)
        }
      }
    }
  }
}
