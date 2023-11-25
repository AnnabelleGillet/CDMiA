package cdmia.ui.view.model

import cdmia.core.categorytheory.morphism.Morphism

class MorphismView(val morphism: Morphism) {
  override def toString: String = morphism.name

  override def equals(that: Any): Boolean = that match
    case morphismView: MorphismView => this.morphism == morphismView.morphism
    case _ => false
}
