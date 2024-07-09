package cdmia.core.categorytheory.pattern.colimit

import cdmia.core.categorytheory.{Category, Object}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.Morphism

import java.lang.Object as JObject

/**
 * A coproduct is a colimit having the following shape, with any number of objects connected to the vertex with morphisms
 * (the vertex begin the domain of the morphism):
 *
 *     a     b
 *      \   /
 *      vertex
 *
 * @param _vertex the [[Object]] that is the vertex of this coproduct.
 * @param objects the [[Object]]s that are part of this coproduct (except the vertex).
 * @param morphisms the [[Morphism]]s that are part of this coproduct.
 */
class Coproduct(_vertex: Object, val objects: Iterable[Object], val morphisms: Iterable[Morphism], name: String = "Coproduct") extends Colimit(_vertex, name) {
  require(objects.toSet.size == objects.size, "A coproduct cannot be built with duplicate objects.")
  require(morphisms.toSet.size == morphisms.size, "A coproduct cannot be built with duplicate morphisms.")
  require(morphisms.forall(_.codomain == vertex), "All the morphisms of the coproduct must have the vertex as codomain.")
  require(objects.forall(o => morphisms.exists(m => m.domain == o)), "All the morphisms of the coproduct must have one of the objects as domain.")
  require(morphisms.forall(m => objects.exists(o => m.domain == o)), "All the objects of the coproduct must be domain of at least one of the morphisms.")
  
  override protected val objectsToCheck: Iterable[Object] = objects
  override protected val morphismsToCheck: Map[Object, Morphism] = morphisms.map(m => m.domain -> m).toMap

  override def getObjects: List[Object] = List[Object](vertex) ::: objects.toList

  override def getMorphisms: List[Morphism] = morphisms.toList

  /**
   * For a coproduct to be valid in a category, all the objects and morphisms must be in the category.
   *
   * @param category the [[Category]] in which to check the validity of this coproduct.
   *  @return true is this coproduct is valid, false otherwise.
   */
  override def isValid(category: Category): Boolean = {
    objects.forall(o => category.objects.exists(_ == o)) &&
      category.objects.exists(_ == vertex) &&
      morphisms.forall(m => category.existsMorphism(m))
  }

  override def explainIsNotValid(category: Category): List[String] = {
    var invalidityReasons: List[String] = List[String]()
    for (obj <- (objects.toList :+ vertex).distinct if !category.objects.exists(_ == obj)) {
      invalidityReasons :+= s"The object $obj does not exist in the category."
    }
    for (morphism <- morphisms if !category.existsMorphism(morphism)) {
      invalidityReasons :+= s"The Morphism $morphism does not exist in the category."
    }
    invalidityReasons
  }

  override def createPatternInDestinationCategory(functor: Functor): Colimit = {
    val vertexTransformation = functor.getDestinationObject(vertex)
    val objectsTransformation = objects.map(functor.getDestinationObject)
    val morphismsTransformation = morphisms.map(functor.getDestinationMorphism)
    new Coproduct(vertexTransformation, objectsTransformation.toSet, morphismsTransformation.toSet)
  }

  override def createPatternsInSourceCategory(functor: Functor): List[Colimit] = {
    val vertexOrigins = functor.getSourceObjects(vertex)
    val morphismOrigins = morphisms.map(m => functor.getSourceMorphisms(m)).toList

    val numberOfPossibleLimits: Int = vertexOrigins.size * morphismOrigins.map(_.size).product
    if (numberOfPossibleLimits <= 0) {
      List[Colimit]()
    } else {
      val morphismCombinations = (for (vertex <- vertexOrigins) yield vertex -> combinationsGenerator(morphismOrigins.map(_.filter(_.codomain == vertex)))).toMap
      (for ((sourceVertex, sourcesMorphisms) <- morphismCombinations; sourceMorphisms <- sourcesMorphisms if sourceMorphisms.size == morphisms.size) yield {
        val sourceObjects = sourceMorphisms.map(_.domain)
        new Coproduct(sourceVertex, sourceObjects, sourceMorphisms)
      }).toList
    }
  }

  private def combinationsGenerator(x: List[List[Morphism]]): List[List[Morphism]] = x match {
    case Nil => List(Nil)
    case h :: _ => if (h.isEmpty) List(Nil) else h.flatMap(i => combinationsGenerator(x.tail).map(i :: _))
  }

  override def equals(obj: Any): Boolean = obj match
    case coproduct: Coproduct => coproduct.vertex == this.vertex &&
      coproduct.objects.forall(o => this.objects.exists(_ == o)) && this.objects.forall(o => coproduct.objects.exists(_ == o)) &&
      coproduct.morphisms.forall(m => this.morphisms.exists(_ == m)) && this.morphisms.forall(m => coproduct.morphisms.exists(_ == m))
    case _ => false
}

