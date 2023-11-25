package cdmia.ui.view.window

import cdmia.datawrapper.modeltransformation.TemplateTransformation
import cdmia.core.categorytheory.{CategoryBuilder, Object}
import cdmia.core.categorytheory.morphism.Morphism
import cdmia.ui.entity.{ModelHandler, TemplateMigrationHandler}
import cdmia.ui.view.model.{CategoryView, FunctorView, PreservationsAndCreationsView}
import javafx.collections.FXCollections
import javafx.geometry.Pos
import javafx.scene.control.ListView
import javafx.scene.layout.{HBox, Pane, VBox}
import javafx.scene.text.Text

import scala.jdk.CollectionConverters.*

class GlobalModelsViewWindow {
  private val functorViewsCache = TemplateMigrationHandler.getAllTemplates().map(t => t -> new FunctorView(t.functor)).toMap
  private val outputViewsCache = TemplateMigrationHandler.getAllTemplates().map(t => t -> {
    new PreservationsAndCreationsView(functorViewsCache(t).sourceView, functorViewsCache(t).destinationView, t.preservationOutputs, t.creationOutputs, t.functor)
  }).toMap

  // Category of models and template transformations
  private val objectModels = ModelHandler.allModels.map((name, model) => model -> new Object(name)).toMap
  private val morphismTransformations = TemplateMigrationHandler.getAllTemplates().map(t => t -> new Morphism(t.name, objectModels(t.source), objectModels(t.destination))).toMap
  private val category = CategoryBuilder("Global view of models and template transformations", objectModels.values, morphismTransformations.values).build()
  private val categoryView = new CategoryView(category)

  private val functorViewPane: Pane = new Pane()
  functorViewPane.getChildren.add({
    val hBoxInitialText = new HBox()
    hBoxInitialText.setMinWidth(MainWindow.TOTAL_CATEGORY_WIDTH)
    hBoxInitialText.setMinHeight((MainWindow.CATEGORY_HEIGHT + MainWindow.FUNCTOR_SEPARATOR_HEIGHT) * 2)
    hBoxInitialText.setAlignment(Pos.CENTER)
    hBoxInitialText.getChildren.add(new Text("Select a template migration to see its effects."))
    hBoxInitialText
  })
  private val outputViewPane: Pane = new Pane()
  outputViewPane.setMinWidth(MainWindow.TOTAL_CATEGORY_WIDTH)
  outputViewPane.setMinHeight(MainWindow.CATEGORY_HEIGHT + MainWindow.FUNCTOR_SEPARATOR_HEIGHT)

  private val migrationsCollection = FXCollections.observableList(TemplateMigrationHandler.getAllTemplates().asJava)
  private val migrationsList = new ListView[TemplateTransformation](migrationsCollection)
  MainWindow.resizeListView(migrationsList, MainWindow.FUNCTOR_SEPARATOR_HEIGHT * 3.2)

  private val vBoxGlobalAndOutputsView = new VBox()
  categoryView.borderPane.getRight.setVisible(false)
  vBoxGlobalAndOutputsView.setSpacing(MainWindow.FUNCTOR_SEPARATOR_HEIGHT)
  vBoxGlobalAndOutputsView.getChildren.addAll(categoryView.borderPane, outputViewPane)

  private val hBox: HBox = new HBox()
  hBox.getChildren.addAll(migrationsList, vBoxGlobalAndOutputsView, functorViewPane)
  private val window = new TemplateWindow(hBox)

  categoryView.init()

  migrationsList.getSelectionModel.selectedItemProperty().addListener((observable, oldValue, newValue) => {
    val functorView = functorViewsCache(newValue)
    functorViewPane.getChildren.clear()
    functorViewPane.getChildren.add(functorView.vBox)
    try {
      functorView.init()
    } catch
      case _ => functorView.update()
    val outputView = outputViewsCache(newValue)
    outputViewPane.getChildren.clear()
    outputViewPane.getChildren.add(outputView.preservationAndCreationPane)
    val newMorphism = morphismTransformations(newValue)
    categoryView.highlightMorphism(newMorphism)
    if (oldValue != null) {
      val oldMorphism = morphismTransformations(oldValue)
      categoryView.removeHighlightOnMorphism(oldMorphism)
    }
    MainWindow.stage.sizeToScene()
  })
}
