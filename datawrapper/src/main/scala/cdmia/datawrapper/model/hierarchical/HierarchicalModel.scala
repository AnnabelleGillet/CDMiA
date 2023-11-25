package cdmia.datawrapper.model.hierarchical

import cdmia.datawrapper.model.Model
import cdmia.core.categorytheory.{Category, CategoryBuilder, Object}
import cdmia.core.categorytheory.morphism.Morphism

object HierarchicalModel extends Model("Hierarchical Model") {
  /*
  Objects
   */
  val tree: Object = new Object("tree")
  val dataType: Object = new Object("dataType")

  override val objects: Iterable[Object] = List[Object](tree, dataType)

  /*
  Morphisms
   */
  val node: Morphism = new Morphism("node", tree, tree)
  val leaf: Morphism = new Morphism("leaf", tree, dataType)

  override val morphisms: Iterable[Morphism] = List[Morphism](node, leaf)

  /*
  Category
   */
  override val category: Category = CategoryBuilder(this.name, objects, morphisms).build()
}
