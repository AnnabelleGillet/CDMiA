package cdmia.core.categorytheory.pattern.limit

import cdmia.core.categorytheory.{Category, Config, Object}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.{Morphism, MorphismComposition}

import java.lang.Object as JObject

/**
 * A product is a limit having the following shape, with any number of objects connected to the vertex with morphisms
 * (the vertex begin the domain of the morphism):
 *
 *      vertex
 *      /   \
 *     a     b
 *
 * @param _vertex the [[Object]] that is the vertex of this product.
 * @param objects the [[Object]]s that are part of this product (except the vertex).
 * @param morphisms the [[Morphism]]s that are part of this product.
 */
class Product(_vertex: Object, val objects: Iterable[Object], val morphisms: Iterable[Morphism], name: String = "Product") extends Limit(_vertex, name) {
  if (!Config.disableRequire) {
    require(objects.toSet.size == objects.size, "A product cannot be built with duplicate objects.")
    require(morphisms.toSet.size == morphisms.size, "A product cannot be built with duplicate morphisms.")
    require(morphisms.forall(_.domain == vertex), "All the morphisms of the product must have the vertex as domain.")
    require(objects.forall(o => morphisms.exists(m => m.codomain == o)), "All the morphisms of the product must have one of the objects as codomain.")
    require(morphisms.forall(m => objects.exists(o => m.codomain == o)), "All the objects of the product must be codomain of at least one of the morphisms.")
  }

  override protected val objectsToCheck: Iterable[Object] = objects
  override protected val morphismsToCheck: Map[Object, Morphism] = morphisms.map(m => m.codomain -> m).toMap

  override def getObjects: List[Object] = List[Object](vertex) ::: objects.toList

  override def getMorphisms: List[Morphism] = morphisms.toList
  
  /**
   * For a product to be valid in a category, all the objects and morphisms must be in the category.
   *
   * @param category the [[Category]] in which to check the validity of this product.
   *  @return true is this product is valid, false otherwise.
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

  override def createPatternInDestinationCategory(functor: Functor): Limit = {
    val vertexTransformation = functor.getDestinationObject(vertex)
    val objectsTransformation = objects.map(functor.getDestinationObject)
    val morphismsTransformation = morphisms.map(functor.getDestinationMorphism)
    new Product(vertexTransformation, objectsTransformation.toSet, morphismsTransformation.toSet)
  }

  override def createPatternsInSourceCategory(functor: Functor): List[Limit] = {
    val vertexOrigins = functor.getSourceObjects(vertex)
    val morphismOrigins = morphisms.map(m => functor.getSourceMorphisms(m)).toList

    val numberOfPossibleLimits: Int = vertexOrigins.size * morphismOrigins.map(_.size).sum
    if (numberOfPossibleLimits <= 0) {
      List[Limit]()
    } else {
      val morphismCombinations = (for (vertex <- vertexOrigins) yield vertex -> combinationsGenerator(morphismOrigins.map(_.filter(_.domain == vertex)))).toMap
      (for ((sourceVertex, sourcesMorphisms) <- morphismCombinations; sourceMorphisms <- sourcesMorphisms if sourceMorphisms.size == morphisms.size) yield {
        val sourceObjects = sourceMorphisms.map(_.codomain)
        new Product(sourceVertex, sourceObjects, sourceMorphisms)
      }).toList
    }
  }

  private def combinationsGenerator[T](x: List[List[T]]): List[List[T]] = x match {
    case Nil => List(Nil)
    case h :: _ => if (h.isEmpty) List(Nil) else h.flatMap(i => combinationsGenerator(x.tail).map(i :: _))
  }

  override def equals(obj: Any): Boolean = obj match
    case product: Product => product.vertex == this.vertex &&
      product.objects.forall(o => this.objects.exists(_ == o)) && this.objects.forall(o => product.objects.exists(_ == o)) &&
      product.morphisms.forall(m => this.morphisms.exists(_ == m)) && this.morphisms.forall(m => product.morphisms.exists(_ == m))
    case _ => false
}
