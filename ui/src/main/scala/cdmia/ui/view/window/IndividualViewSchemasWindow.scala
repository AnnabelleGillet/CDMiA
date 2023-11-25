package cdmia.ui.view.window

import cdmia.datawrapper.model.Model
import cdmia.datawrapper.schema.Schema
import cdmia.ui.entity.SchemaHandler
import cdmia.ui.view.model.FunctorView
import javafx.geometry.Pos
import javafx.scene.control.{TreeCell, TreeItem, TreeView}
import javafx.scene.layout.{HBox, Pane}
import javafx.scene.text.Text

class IndividualViewSchemasWindow {
  private val viewPane: Pane = new Pane()
  viewPane.getChildren.add({
    val hBoxInitialText = new HBox()
    
    hBoxInitialText.setMinWidth(MainWindow.TOTAL_CATEGORY_WIDTH)
    hBoxInitialText.setMinHeight((MainWindow.CATEGORY_HEIGHT + MainWindow.FUNCTOR_SEPARATOR_HEIGHT) * 2)
    hBoxInitialText.setAlignment(Pos.CENTER)
    hBoxInitialText.getChildren.add(new Text("Select a schema"))
    hBoxInitialText
  })

  // Cache the views
  private val cacheOutput: Map[Schema, FunctorView] = SchemaHandler.getAllSchemasByModel().map((model, schemas) => {
    for (schema <- schemas) yield {
      schema -> new FunctorView(schema.functorTowardsModel)
    }
  }).flatten.toMap

  // Create tree view
  private val tree: TreeView[Object] = new TreeView[Object]()
  private val rootItem = new TreeItem[Object]()
  for ((model, schemas) <- SchemaHandler.getAllSchemasByModel()) {
    val modelTreeItem = new TreeItem[Object](model)
    for (schema <- schemas) {
      val schemaTreeItem = new TreeItem[Object](schema)
      modelTreeItem.getChildren.add(schemaTreeItem)
    }
    rootItem.getChildren.add(modelTreeItem)
    modelTreeItem.setExpanded(true)
  }
  tree.setRoot(rootItem)
  tree.setShowRoot(false)
  tree.setPrefWidth(200)

  // Modify the default name of items
  tree.setCellFactory(item => {
    new TreeCell[Object]() {
      prefWidthProperty().bind(tree.widthProperty())
      setMaxWidth(getPrefWidth)
      setWidth(getPrefWidth)

      override def updateItem(item: Object, empty: Boolean): Unit = {
        super.updateItem(item, empty)
        this.setWrapText(true)
        setText({
          if (empty || item == null) {
            ""
          } else {
            item.asInstanceOf[Any] match
              case model: Model => model.name
              case schema: Schema => schema.name
              case _ => item.toString
          }
        })
      }
    }
  })

  // Add listener to display schema when selected
  tree.getSelectionModel.selectedItemProperty().addListener((observable, oldValue, newValue) => {
    val value = newValue.getValue
    value match
      case model: Model => ()
      case schema: Schema => {
        val functorView = cacheOutput(schema)
        viewPane.getChildren.clear()
        viewPane.getChildren.add(functorView.vBox)
        try {
          functorView.init()
        } catch
          case _ => functorView.update()

        MainWindow.stage.sizeToScene()
      }
      case _ => ()
  })

  // Display window
  private val hBox: HBox = new HBox()
  hBox.getChildren.addAll(tree, viewPane)
  private val window = new TemplateWindow(hBox)
}
