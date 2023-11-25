package cdmia.datawrapper.model

import cdmia.datawrapper.modeltransformation.{Creation, Preservation}
import cdmia.core.categorytheory.{Category, Object, ObjectTransformation}
import cdmia.core.categorytheory.morphism.{Isomorphism, Morphism, MorphismEquality, MorphismTransformation}
import cdmia.core.categorytheory.pattern.colimit.Colimit
import cdmia.core.categorytheory.pattern.limit.Limit

/**
 * An abstract class that must be used when declaring a new data model.
 *
 * @param name: the name of this model.
 */
abstract class Model(val name: String) extends Serializable {
  val objects: Iterable[Object]
  val morphisms: Iterable[Morphism]
  val category: Category

  val preservationVerifications: List[Preservation] = List[Preservation]()
  val creationVerifications: List[Creation] = List[Creation]()
}

object Model {
  trait Element extends Serializable {
    val objects: List[Object] = List[Object]()
    val morphisms: List[Morphism] = List[Morphism]()
    val pathEqualities: List[MorphismEquality] = List[MorphismEquality]()
    val isomorphisms: List[Isomorphism] = List[Isomorphism]()
    val limits: List[Limit] = List[Limit]()
    val colimits: List[Colimit] = List[Colimit]()

    val objectTransformations: List[ObjectTransformation] = List[ObjectTransformation]()
    val morphismTransformations: List[MorphismTransformation] = List[MorphismTransformation]()

    val preservationVerifications: List[Preservation] = List[Preservation]()
  }
}
