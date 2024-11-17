package cdmia.datawrapper.datasource

import cdmia.core.categorytheory.Object
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.Morphism
import org.scalatest.funsuite.*
abstract class DatasourceTest extends AnyFunSuite {
  case class ExpectedMorphism(name: String, domain: Object, codomain: Object)

  /**
   * Check if the obtained objects are the same as the expected objects.
   */
  protected def checkObjects(expectedObjectNames: List[String], obtainedObjects: Map[String, Object]): Unit = {
    assertResult(expectedObjectNames.size)(obtainedObjects.size)
    assertResult(true)(expectedObjectNames.forall(obtainedObjects.contains))
    assertResult(true)(obtainedObjects.keys.forall(expectedObjectNames.contains))
  }

  /**
   * Check if the obtained morphisms are the same as the expected morphisms.
   */
  protected def checkMorphisms(expectedMorphisms: List[ExpectedMorphism], obtainedMorphisms: Map[String, Morphism]): Unit = {
    assertResult(expectedMorphisms.size)(obtainedMorphisms.size)
    for (em <- expectedMorphisms) {
      assertResult(true)(obtainedMorphisms.contains(em.name))
      assertResult(em.name)(obtainedMorphisms(em.name).name)
      assertResult(em.domain)(obtainedMorphisms(em.name).domain)
      assertResult(em.codomain)(obtainedMorphisms(em.name).codomain)
    }
  }

  /**
   * Check if the obtained object transformations are the same as the expected object transformations.
   */
  protected def checkObjectTransformations(functor: Functor, expectedObjectTransformations: Map[Object, Object]): Unit = {
    for (ot <- functor.objectTransformations) {
      assertResult(expectedObjectTransformations(ot.source))(ot.destination)
    }
  }

  /**
   * Check if the obtained morphism transformations are the same as the expected morphism transformations.
   */
  protected def checkMorphismTransformations(functor: Functor, expectedMorphismTransformations: Map[Morphism, Morphism]): Unit = {
    for (mt <- functor.morphismTransformations) {
      assertResult(expectedMorphismTransformations(mt.source))(mt.destination)
    }
  }
}
