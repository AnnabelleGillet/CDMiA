package cdmia.ui.view.window

import cdmia.datawrapper.model.Model
import cdmia.datawrapper.modeltransformation.SchemaModelTransformation
import cdmia.datawrapper.schema.Schema
import cdmia.ui.entity.{ModelHandler, SchemaHandler}
import cdmia.ui.view.model.{CategoryView, FunctorView, TransformationView}
import javafx.collections.FXCollections
import javafx.geometry.Pos
import javafx.scene.{control, layout}
import javafx.scene.control.{Button, ListView, TreeCell, TreeItem, TreeView}
import javafx.scene.layout.{HBox, Pane, VBox}
import javafx.scene.text.Text

import scala.jdk.CollectionConverters.*

abstract class ApplyMigrationWindow {
  protected var selectedSchema: Option[Schema] = None
  protected var selectedDestinationModel: Option[Model] = None

  /*
  Schema view
   */
  private val viewSchemaPane: Pane = new Pane()
  viewSchemaPane.getChildren.add({
    val hBoxInitialText = new HBox()

    hBoxInitialText.setMinWidth(MainWindow.TOTAL_CATEGORY_WIDTH)
    hBoxInitialText.setMinHeight((MainWindow.CATEGORY_HEIGHT + MainWindow.FUNCTOR_SEPARATOR_HEIGHT) * 2)
    hBoxInitialText.setAlignment(Pos.CENTER)
    hBoxInitialText.getChildren.add(new Text("Select a schema"))
    hBoxInitialText
  })

  // Cache the views
  private val cacheSchemaOutput: Map[Schema, FunctorView] = SchemaHandler.getAllSchemasByModel().map((model, schemas) => {
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
      case schema: Schema => {
        val functorView = cacheSchemaOutput(schema)
        viewSchemaPane.getChildren.clear()
        viewSchemaPane.getChildren.add(functorView.vBox)
        try {
          functorView.init()
        } catch
          case _ => functorView.update()
        setSelectedSchema(schema)
      }
      case _ => ()
  })

  /*
  Destination model view
   */
  private val vBoxDestinationModel: HBox = new HBox()
  private val viewDestinationModelPane: Pane = new Pane()
  viewDestinationModelPane.getChildren.add({
    val hBoxInitialText = new HBox()

    hBoxInitialText.setMinWidth(MainWindow.TOTAL_CATEGORY_WIDTH)
    hBoxInitialText.setMinHeight(MainWindow.CATEGORY_HEIGHT + MainWindow.FUNCTOR_SEPARATOR_HEIGHT)
    hBoxInitialText.setAlignment(Pos.CENTER)
    hBoxInitialText.getChildren.add(new Text("Select a destination model"))
    hBoxInitialText
  })

  // Cache the views
  private val cacheDestinationModelOutput: Map[String, CategoryView] = ModelHandler.allCategories.map((name, category) => {
    name -> new CategoryView(category)
  })

  private val modelCollection = FXCollections.observableList(ModelHandler.allCategories.keySet.toList.sorted.asJava)
  private val modelList = new ListView[String](modelCollection)
  MainWindow.resizeListView(modelList, MainWindow.FUNCTOR_SEPARATOR_HEIGHT * 2)

  modelList.getSelectionModel.selectedItemProperty().addListener((observable, oldValue, newValue) => {
    val categoryView = cacheDestinationModelOutput(newValue)
    viewDestinationModelPane.getChildren.clear()
    viewDestinationModelPane.getChildren.add(categoryView.borderPane)
    try {
      categoryView.init()
    } catch
      case _ => categoryView.update()
    setSelectedDestinationModel(ModelHandler.allModels(newValue))
  })

  vBoxDestinationModel.getChildren.addAll(viewDestinationModelPane, modelList)

  // Display window
  private val vBoxModelAndMigration = new VBox()
  protected val migrationViewPane = new VBox()
  vBoxModelAndMigration.getChildren.addAll(vBoxDestinationModel, migrationViewPane)
  private val hBox: HBox = new HBox()
  hBox.getChildren.addAll(tree, viewSchemaPane, vBoxModelAndMigration)
  private val window = new TemplateWindow(hBox)

  protected def setSelectedSchema(schema: Schema): Unit = {
    selectedSchema = Some(schema)
    window.stage.sizeToScene()
  }

  protected def setSelectedDestinationModel(model: Model): Unit = {
    selectedDestinationModel = Some(model)
    window.stage.sizeToScene()
  }

  protected def applyMigration(migration: SchemaModelTransformation): Unit = {
    val migrationView = new TransformationView(migration)
    val newVBox = new VBox()
    newVBox.setAlignment(Pos.CENTER)
    val modify = new Button("Modify migration")
    modify.setOnAction(value => {
      window.switchContent(hBox)
    })
    modify.setFont(MainWindow.TITLE_FONT)
    newVBox.getChildren.addAll(migrationView.vBox, modify)
    window.switchContent(newVBox)
    migrationView.init()
  }
}
