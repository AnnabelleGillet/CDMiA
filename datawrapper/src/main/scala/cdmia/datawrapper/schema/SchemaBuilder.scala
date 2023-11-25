package cdmia.datawrapper.schema

import cdmia.datawrapper.model.Model
import cdmia.core.categorytheory.{Category, CategoryBuilder}
import cdmia.core.categorytheory.functor.Functor

/**
 * Abstract class to implement for a schema builder specialized for a data model.
 *
 * @param name: the name of the schema.
 */
abstract class SchemaBuilder(val name: String) extends Serializable {
  lazy val elements: List[Model.Element]

  /**
   * Build the schema corresponding to the information given to this builder.
   *
   * @return the corresponding [[Schema]].
   */
  def build(): Schema

  /**
   * Build the category of the schema corresponding to the information given to this builder.
   *
   * @return the [[Category]] of the schema.
   */
  protected def buildCategory(): Category = {
    CategoryBuilder(name, elements.flatMap(_.objects), elements.flatMap(_.morphisms))
      .withMorphismEqualities(elements.flatMap(_.pathEqualities))
      .withIsomorphisms(elements.flatMap(_.isomorphisms))
      .build(elements.flatMap(_.limits), elements.flatMap(_.colimits))
  }

  /**
   * Build the functor mapping the schema to its model corresponding to the information given to this builder.
   *
   * @param category: the [[Category]] of the schema.
   * @param model: the [[Model]] of the schema.
   * @return the [[Functor]] mapping the schema to its model.
   */
  protected def buildFunctor(category: Category, model: Model): Functor = {
    new Functor(s"$name Model", category, model.category, elements.flatMap(_.objectTransformations), elements.flatMap(_.morphismTransformations))
  }
}
