package cdmia.core.categorytheory.pattern.colimit

import cdmia.core.categorytheory.{Category, Object}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.Morphism

/**
 * An object is initial if, for every object x of C, there exists a unique morphism !:vertex -> x.
 *
 * @param _vertex the initial [[Object]].
 */
class InitialObject(_vertex: Object, name: String = "InitialObject") extends Colimit(_vertex, name) {
  override protected val objectsToCheck: Iterable[Object] = Iterable[Object]()
  override protected val morphismsToCheck: Map[Object, Morphism] = Map[Object, Morphism]()

  override def getObjects: List[Object] = List[Object](vertex)

  override def getMorphisms: List[Morphism] = List[Morphism](vertex.identityMorphism)

  /**
   * For an initial to be valid in a category, the vertex must be in the category.
   *
   * @param category the [[Category]] in which to check the validity of this initial object.
   * @return true is this initial object is valid, false otherwise.
   */
  override def isValid(category: Category): Boolean = {
    category.objects.exists(_ == vertex)
  }

  override def explainIsNotValid(category: Category): List[String] = {
    if (!category.objects.exists(_ == vertex)) {
      List[String](s"The object $vertex does not exist in the category.")
    } else {
      List[String]()
    }
  }

  override def createPatternInDestinationCategory(functor: Functor): Colimit = {
    val vertexTransformation = functor.getDestinationObject(vertex)
    new InitialObject(vertexTransformation)
  }

  override def createPatternsInSourceCategory(functor: Functor): List[Colimit] = {
    val vertexOrigins = functor.getSourceObjects(vertex)
    vertexOrigins.map(new InitialObject(_))
  }

  override def equals(obj: Any): Boolean = obj match
    case t: InitialObject => t.vertex == this.vertex
    case _ => false

  override def toString: String = s"InitialObject(vertex = $vertex)"
}
