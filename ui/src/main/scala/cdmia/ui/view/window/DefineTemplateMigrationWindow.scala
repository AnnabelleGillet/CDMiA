package cdmia.ui.view.window

import cdmia.datawrapper.model.Model
import cdmia.datawrapper.modeltransformation.TemplateTransformation
import cdmia.ui.entity.{ModelHandler, TemplateMigrationHandler}
import cdmia.ui.view.model.{CategoryView, FunctorDefinitionView}
import javafx.collections.FXCollections
import javafx.geometry.Pos
import javafx.scene.control
import javafx.scene.control.{Alert, ListView, TextInputDialog}
import javafx.scene.layout.{AnchorPane, HBox, VBox}
import javafx.scene.text.Text

import scala.jdk.CollectionConverters.*

/**
 * Window to define a migration between two models.
 */
class DefineTemplateMigrationWindow {
  private var selectedSourceModel: Option[Model] = None
  private var selectedDestinationModel: Option[Model] = None

  // Cache the views
  private val cacheSourceModelOutput: Map[String, CategoryView] = ModelHandler.allCategories.map((name, category) => {
    name -> new CategoryView(category)
  })
  private val cacheDestinationModelOutput: Map[String, CategoryView] = ModelHandler.allCategories.map((name, category) => {
    name -> new CategoryView(category)
  })

  private val modelCollection = FXCollections.observableList(ModelHandler.allCategories.keySet.toList.sorted.asJava)

  /*
  Source model view
   */
  private val vBoxSourceModel: HBox = new HBox()
  private val viewSourceModelPane: AnchorPane = new AnchorPane()
  viewSourceModelPane.getChildren.add({
    val hBoxInitialText = new HBox()

    hBoxInitialText.setMinWidth(MainWindow.TOTAL_CATEGORY_WIDTH)
    hBoxInitialText.setMinHeight(MainWindow.CATEGORY_HEIGHT + MainWindow.FUNCTOR_SEPARATOR_HEIGHT * 2)
    hBoxInitialText.setAlignment(Pos.CENTER)
    hBoxInitialText.getChildren.add(new Text("Select a source model"))
    hBoxInitialText
  })

  private val sourceModelList = new ListView[String](modelCollection)
  MainWindow.resizeListView(sourceModelList, MainWindow.FUNCTOR_SEPARATOR_HEIGHT * 2)

  sourceModelList.getSelectionModel.selectedItemProperty().addListener((observable, oldValue, newValue) => {
    val categoryView = cacheSourceModelOutput(newValue)
    viewSourceModelPane.getChildren.clear()
    viewSourceModelPane.getChildren.add(categoryView.borderPane)
    try {
      categoryView.init()
    } catch
      case _ => categoryView.update()
    setSelectedSourceModel(ModelHandler.allModels(newValue))
  })

  vBoxSourceModel.getChildren.addAll(sourceModelList, viewSourceModelPane)

  /*
  Destination model view
   */
  private val vBoxDestinationModel: HBox = new HBox()
  private val viewDestinationModelPane: AnchorPane = new AnchorPane()
  viewDestinationModelPane.getChildren.add({
    val hBoxInitialText = new HBox()

    hBoxInitialText.setMinWidth(MainWindow.TOTAL_CATEGORY_WIDTH)
    hBoxInitialText.setMinHeight(MainWindow.CATEGORY_HEIGHT + MainWindow.FUNCTOR_SEPARATOR_HEIGHT * 2)
    hBoxInitialText.setAlignment(Pos.CENTER)
    hBoxInitialText.getChildren.add(new Text("Select a destination model"))
    hBoxInitialText
  })

  private val destinationModelList = new ListView[String](modelCollection)
  MainWindow.resizeListView(destinationModelList, MainWindow.FUNCTOR_SEPARATOR_HEIGHT * 2)

  destinationModelList.getSelectionModel.selectedItemProperty().addListener((observable, oldValue, newValue) => {
    val categoryView = cacheDestinationModelOutput(newValue)
    viewDestinationModelPane.getChildren.clear()
    viewDestinationModelPane.getChildren.add(categoryView.borderPane)
    try {
      categoryView.init()
    } catch
      case _ => categoryView.update()
    setSelectedDestinationModel(ModelHandler.allModels(newValue))
  })

  vBoxDestinationModel.getChildren.addAll(viewDestinationModelPane, destinationModelList)

  /*
  Functor definition view
   */
  protected val migrationViewPane = new VBox()
  migrationViewPane.setAlignment(Pos.CENTER)
  migrationViewPane.setMinWidth(MainWindow.CATEGORY_WIDTH)
  migrationViewPane.setMinHeight(MainWindow.CATEGORY_HEIGHT)
  migrationViewPane.getChildren.add(new Text("Select a source and a destination model to define a template migration."))

  /*
   Display window
   */
  private val hBoxModels = new HBox()
  hBoxModels.getChildren.addAll(vBoxSourceModel, vBoxDestinationModel)
  private val vBox: VBox = new VBox()
  vBox.setAlignment(Pos.CENTER)
  vBox.getChildren.addAll(hBoxModels, migrationViewPane)
  private val window = new TemplateWindow(vBox)

  private def setSelectedSourceModel(model: Model): Unit = {
    selectedSourceModel = Some(model)
    setMigrationPane()
    window.stage.sizeToScene()
  }

  private def setSelectedDestinationModel(model: Model): Unit = {
    selectedDestinationModel = Some(model)
    setMigrationPane()
    window.stage.sizeToScene()
  }

  private def setMigrationPane(): Unit = {
    if (selectedSourceModel.isDefined && selectedDestinationModel.isDefined) {
      val functorDefinitionView = new FunctorDefinitionView(selectedSourceModel.get.category, selectedDestinationModel.get.category)
      migrationViewPane.getChildren.clear()
      migrationViewPane.getChildren.addAll(functorDefinitionView.vBox, functorDefinitionView.validateButton)
      functorDefinitionView.validateButton.setOnAction(event => {
        val functor = functorDefinitionView.getFunctor
        val dialog = new TextInputDialog()
        dialog.setContentText("Enter a name for the template migration:")
        val result = dialog.showAndWait()
        if (!result.isPresent) {
          val alert = new Alert(Alert.AlertType.ERROR)
          alert.setHeaderText(null)
          alert.setContentText("Name cannot be empty. Template migration was not created.")
          alert.showAndWait()
        } else {
          val templateMigration = new TemplateTransformation(result.get(), selectedSourceModel.get, selectedDestinationModel.get, functor)
          TemplateMigrationHandler.addTemplate(templateMigration)
          val alert = new Alert(Alert.AlertType.CONFIRMATION)
          alert.setHeaderText(null)
          alert.setContentText("Template migration correctly created!")
          alert.showAndWait()
        }
      })
    }
  }
}
