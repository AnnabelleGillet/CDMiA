package cdmia.ui.view.window

import javafx.scene.control.{Menu, MenuBar, MenuItem}
import javafx.scene.{Group, Scene, layout}
import javafx.scene.layout.{Pane, VBox}
import javafx.scene.text.Text
import javafx.stage.Stage

/**
 * Structure of the template window with the menu.
 *
 * @param content the specific content of the window.
 */
class TemplateWindow(val content: Pane) {
  private val menu: MenuBar = new MenuBar()

  private val modelsMenu: Menu = new Menu("Models")
  private val globalViewModels: MenuItem = newMenuItem("Global view")
  private val individualViewModels: MenuItem = newMenuItem("Individual view")
  globalViewModels.setOnAction(value => {
    new GlobalModelsViewWindow()
  })
  individualViewModels.setOnAction(value => {
    new IndividualViewModelsWindow()
  })
  modelsMenu.getItems.addAll(globalViewModels, individualViewModels)

  private val schemasMenu: Menu = new Menu("Schemas")
  private val addSchema: MenuItem = newMenuItem("Add a schema")
  private val individualViewSchemas: MenuItem = newMenuItem("See a schema")
  addSchema.setOnAction(value => {
    // TODO
  })
  individualViewSchemas.setOnAction(value => {
    new IndividualViewSchemasWindow()
  })
  schemasMenu.getItems.addAll(/*addSchema,*/ individualViewSchemas)

  private val migrationMenu: Menu = new Menu("Migration")
  private val seeTemplateMigration: MenuItem = newMenuItem("See a template migration")
  private val defineTemplateMigration: MenuItem = newMenuItem("Define a template migration")
  private val applyTemplateMigration: MenuItem = newMenuItem("Apply a template migration")
  private val defineMigration: MenuItem = newMenuItem("Define a custom migration")
  seeTemplateMigration.setOnAction(value => {
    new GlobalModelsViewWindow()
  })
  defineTemplateMigration.setOnAction(value => {
    new DefineTemplateMigrationWindow()
  })
  applyTemplateMigration.setOnAction(value => {
    new ApplyTemplateMigrationWindow()
  })
  defineMigration.setOnAction(value => {
    new ApplyCustomMigrationWindow()
  })
  migrationMenu.getItems.addAll(seeTemplateMigration, defineTemplateMigration, applyTemplateMigration, defineMigration)

  menu.getMenus.setAll(modelsMenu, schemasMenu, migrationMenu)

  private val vBox: VBox = new VBox()
  vBox.getChildren.addAll(menu, content)

  private val scene: Scene = new Scene(new Group(vBox))
  scene.getStylesheets.add("css/smartgraph.css")
  scene.getStylesheets.add("css/modena.css")
  scene.getStylesheets.add("css/application.css")

  scene.getRoot.getTransforms.setAll(MainWindow.scale)

  val stage: Stage = MainWindow.stage
  stage.setResizable(false)
  stage.setTitle("Categorical Data Migration Assessor")
  stage.setScene(scene)
  stage.show()
  stage.sizeToScene()

  def switchContent(newContent: Pane): Unit = {
    vBox.getChildren.remove(1)
    vBox.getChildren.add(newContent)
    stage.sizeToScene()
  }

  private def newMenuItem(text: String): MenuItem = {
    val menuItem: MenuItem = new MenuItem()
    val textContent = new Text(text)
    textContent.getTransforms.setAll(MainWindow.scale)
    menuItem.setGraphic(new Group(textContent))
    menuItem
  }
}
