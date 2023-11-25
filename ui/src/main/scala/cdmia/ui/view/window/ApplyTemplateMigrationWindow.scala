package cdmia.ui.view.window

import cdmia.datawrapper.model.Model
import cdmia.datawrapper.modeltransformation.{SchemaModelTransformation, TemplateTransformation}
import cdmia.datawrapper.schema.Schema
import cdmia.ui.entity.TemplateMigrationHandler
import javafx.collections.FXCollections
import javafx.geometry.Pos
import javafx.scene.control.{Button, ComboBox, ListCell}
import javafx.scene.layout.HBox
import javafx.scene.text.Text

import scala.jdk.CollectionConverters.*

class ApplyTemplateMigrationWindow extends ApplyMigrationWindow {
  migrationViewPane.setAlignment(Pos.CENTER)
  private val templatesComboBox = new ComboBox[TemplateTransformation]()
  templatesComboBox.setPromptText("--Select a template--")
  // Custom cell factory to scale text
  templatesComboBox.setCellFactory(value => {
    new ListCell[TemplateTransformation]() {
      override def updateItem(item: TemplateTransformation, empty: Boolean): Unit = {
        super.updateItem(item, empty)
        if (item != null && !empty) {
          this.getListView.getTransforms.setAll(MainWindow.scale)
          setText(item.toString)
        } else {
          setText(null)
        }
      }
    }
  })
  private val button = new Button("Apply template migration")
  button.setOnAction(event => {
    if (templatesComboBox.getValue != null) {
      applyMigration(new SchemaModelTransformation(selectedSchema.get, templatesComboBox.getValue))
    }
  })
  migrationViewPane.setMinWidth(MainWindow.CATEGORY_WIDTH)
  migrationViewPane.setMinHeight(MainWindow.CATEGORY_HEIGHT)

  private def updateComboBox(): Unit = {
    if (selectedSchema.isDefined && selectedDestinationModel.isDefined) {
      val availableTemplate = TemplateMigrationHandler.getTemplates(selectedSchema.get.model, selectedDestinationModel.get)
      if (availableTemplate.isEmpty) {
        migrationViewPane.getChildren.clear()
        migrationViewPane.getChildren.add(new Text(s"No available migration template from ${selectedSchema.get.model.name} to ${selectedDestinationModel.get.name}."))
      } else {
        templatesComboBox.setItems(FXCollections.observableList(availableTemplate.asJava))
        migrationViewPane.getChildren.clear()
        migrationViewPane.getChildren.add({
          val hBox = new HBox()
          hBox.getChildren.addAll(templatesComboBox, button)
          hBox.setSpacing(20)
          hBox
        })
      }
    }
  }

  override protected def setSelectedSchema(schema: Schema): Unit = {
    super.setSelectedSchema(schema)
    updateComboBox()
  }

  override protected def setSelectedDestinationModel(model: Model): Unit = {
    super.setSelectedDestinationModel(model)
    updateComboBox()
  }
}
