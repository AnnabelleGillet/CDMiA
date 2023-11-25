package cdmia.ui.view.window

import cdmia.ui.entity.ModelHandler
import cdmia.ui.view.model.CategoryView
import javafx.collections.FXCollections
import javafx.geometry.Pos
import javafx.scene.control.ListView
import javafx.scene.layout.{HBox, Pane}
import javafx.scene.text.Text

import scala.jdk.CollectionConverters.*

/**
 * Window that display the categories of the data models, and allow to select which one to see.
 */
class IndividualViewModelsWindow() {
  private val viewPane: Pane = new Pane()
  viewPane.getChildren.add({
    val hBoxInitialText = new HBox()
    hBoxInitialText.setMinWidth(MainWindow.TOTAL_CATEGORY_WIDTH)
    hBoxInitialText.setMinHeight(MainWindow.CATEGORY_HEIGHT + MainWindow.FUNCTOR_SEPARATOR_HEIGHT)
    hBoxInitialText.setAlignment(Pos.CENTER)
    hBoxInitialText.getChildren.add(new Text("Select a model"))
    hBoxInitialText
  })

  // Cache the views
  private val cacheOutput: Map[String, CategoryView] = ModelHandler.allCategories.map((name, category) => {
    name -> new CategoryView(category)
  })

  private val modelCollection = FXCollections.observableList(ModelHandler.allCategories.keySet.toList.sorted.asJava)
  private val modelList = new ListView[String](modelCollection)
  MainWindow.resizeListView(modelList, MainWindow.FUNCTOR_SEPARATOR_HEIGHT * 2)

  private val hBox: HBox = new HBox()
  hBox.getChildren.addAll(modelList, viewPane)
  private val window = new TemplateWindow(hBox)

  modelList.getSelectionModel.selectedItemProperty().addListener((observable, oldValue, newValue) => {
    val categoryView = cacheOutput(newValue)
    viewPane.getChildren.clear()
    viewPane.getChildren.add(categoryView.borderPane)
    try {
      categoryView.init()
    } catch
      case _ => categoryView.update()
    window.stage.sizeToScene()
  })
}
