package cdmia.core.categorytheory.checker

import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.Isomorphism

import java.lang.{Object => JObject}

/**
 * Check if there is a natural isomorphism between two functors. To use this class, the two functors
 * must have the same category domain and codomain.
 *
 * @param from: the source [[Functor]].
 * @param to: the destination [[Functor]]
 */
class NaturalIsomorphismChecker(from: Functor, to: Functor) {
  require(from.domain == to.domain, s"Both functors must have the same category domain, but got ${from.domain} and ${to.domain}.")
  require(from.codomain == to.codomain, s"Both functors must have the same category codomain, but got ${from.codomain} and ${to.codomain}.")

  private val naturalTransformationCheckerFromTo = new NaturalTransformationChecker(from, to)
  private val naturalTransformationCheckerToFrom = new NaturalTransformationChecker(to, from)

  private val solutions: List[Map[Object, Isomorphism]] = {
    var _solutions: List[Map[Object, Isomorphism]] = List[Map[Object, Isomorphism]]()
    for (solutionFromTo <- naturalTransformationCheckerFromTo.alphaSolutions;
         solutionToFrom <- naturalTransformationCheckerToFrom.alphaSolutions) {
      var validSolution: Boolean = true
      var solution: Map[Object, Isomorphism] = Map[Object, Isomorphism]()
      for ((x, alpha) <- solutionFromTo if validSolution) {
        validSolution = validSolution && from.codomain.isAnIsomorphism(alpha, solutionToFrom(x))
        solution += x -> new Isomorphism(alpha, solutionToFrom(x))
      }
      if (validSolution) {
        _solutions :+= solution
      }
    }
    _solutions
  }

  val isValid: Boolean = solutions.nonEmpty
}
