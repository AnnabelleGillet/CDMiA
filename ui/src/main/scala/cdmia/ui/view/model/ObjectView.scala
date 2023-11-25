package cdmia.ui.view.model

import cdmia.core.categorytheory.Object

class ObjectView(val obj: Object) {
  override def toString: String = obj.name

  override def equals(that: Any): Boolean = that match
    case objectView: ObjectView => this.obj == objectView.obj
    case _ => false
}
