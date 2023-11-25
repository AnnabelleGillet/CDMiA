package cdmia.ui.view.window

import cdmia.datawrapper.model.Model
import cdmia.datawrapper.modeltransformation.SchemaModelTransformation
import cdmia.datawrapper.schema.Schema
import cdmia.ui.view.model.FunctorDefinitionView
import javafx.geometry.Pos

class ApplyCustomMigrationWindow extends ApplyMigrationWindow {
  migrationViewPane.setAlignment(Pos.CENTER)
  migrationViewPane.setMinWidth(MainWindow.CATEGORY_WIDTH)
  migrationViewPane.setMinHeight(MainWindow.CATEGORY_HEIGHT)

  private def updateDefineFunctorPane(): Unit = {
    if (selectedSchema.isDefined && selectedDestinationModel.isDefined) {
      val defineFunctorView = new FunctorDefinitionView(selectedSchema.get.category, selectedDestinationModel.get.category)
      defineFunctorView.validateButton.setOnAction(event => {
        applyMigration(new SchemaModelTransformation(selectedSchema.get, selectedDestinationModel.get, defineFunctorView.getFunctor))
      })

      migrationViewPane.getChildren.clear()
      migrationViewPane.getChildren.addAll(defineFunctorView.vBox, defineFunctorView.validateButton)

    }
  }

  override protected def setSelectedSchema(schema: Schema): Unit = {
    super.setSelectedSchema(schema)
    updateDefineFunctorPane()
  }

  override protected def setSelectedDestinationModel(model: Model): Unit = {
    super.setSelectedDestinationModel(model)
    updateDefineFunctorPane()
  }
}
