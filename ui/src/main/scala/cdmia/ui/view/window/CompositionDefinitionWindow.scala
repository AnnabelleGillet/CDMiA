package cdmia.ui.view.window

import cdmia.core.categorytheory.{Category, Object}
import cdmia.core.categorytheory.morphism.Morphism
import javafx.collections.FXCollections
import javafx.geometry.Pos
import javafx.scene.{Group, Scene}
import javafx.scene.control.{Button, ComboBox, ListCell}
import javafx.scene.layout.{GridPane, HBox, VBox}
import javafx.stage.Stage
import javafx.scene.text.Text

import scala.jdk.CollectionConverters.*

/**
 * A window allowing to build a composition from a domain object to a codomain object within a category.
 *
 * @param category the [[Category]] in which to build a composition.
 * @param domain the [[Object]] domain of the composition.
 * @param codomain the [[Object]] codomain of the composition.
 */
class CompositionDefinitionWindow(val category: Category, val domain: Object, val codomain: Object) {
  require(domain.isInCategory(category), s"The domain $domain must be in the category.")
  require(codomain.isInCategory(category), s"The domain $codomain must be in the category.")
  require(category.existsMorphism(domain, codomain), s"$domain must be linked to $codomain in $category")

  private val MAX_COLUMNS: Int = 5
  private var nbOfMorphisms: Int = 0

  private var composition: List[Morphism] = List[Morphism]()

  var compositionIsDefined: Boolean = false

  private val resultPreview = new Text()
  private val validateButton = new Button("Validate")
  validateButton.setDisable(true)

  // Pane to show domain object - composition - codomain object
  private val hBox = new HBox()
  hBox.setAlignment(Pos.CENTER)
  private val gridComposition = new GridPane()
  hBox.getChildren.addAll(new Text(domain.name), gridComposition, new Text(codomain.name))

  // Pane to show previous hbox - current composition - button to validate the selection
  private val vBox = new VBox()
  vBox.setAlignment(Pos.CENTER)
  vBox.getChildren.addAll(hBox, resultPreview, validateButton)

  gridComposition.add(chooseMorphismPane(domain, 0), 0, 0)

  val stage = new Stage()
  private val scene = new Scene(new Group(vBox))
  scene.getStylesheets.add("css/application.css")
  scene.getRoot.getTransforms.setAll(MainWindow.scale)
  stage.setScene(scene)
  stage.show()

  validateButton.setOnAction(event => {
    compositionIsDefined = true
    stage.close()
  })

  private def chooseMorphismPane(domain: Object, index: Int): VBox = {
    val comboBox = new ComboBox[Morphism]()
    comboBox.setPromptText("--Select a morphism--")
    // Select only morphisms that have as codomain an object for which a path exists to the destination object
    comboBox.setItems(FXCollections.observableList(category.morphisms.concat(category.identityMorphisms).filter(m => m.domain == domain && category.existsMorphism(m.codomain, codomain)).toList.asJava))
    // Custom cell factory to scale text
    comboBox.setCellFactory(value => {
      new ListCell[Morphism]() {
        override def updateItem(item: Morphism, empty: Boolean): Unit = {
          super.updateItem(item, empty)
          if (item != null && !empty) {
            this.getListView.getTransforms.setAll(MainWindow.scale)
            setText(s"${item.name}: ${item.domain.name} -> ${item.codomain.name}")
          } else {
            setText(null)
          }
        }
      }
    })
    comboBox.setButtonCell(comboBox.getCellFactory.call(null))
    val addButton = new Button("Add morphism")
    val deleteButton = new Button("Delete from here")
    deleteButton.setDisable(true)
    deleteButton.setVisible(false)
    comboBox.setOnAction(value => {
      if (comboBox.getValue != null) {
        addButton.setDisable(false)
      } else {
        addButton.setDisable(true)
      }
    })
    addButton.setDisable(true)
    addButton.setOnAction(event => {
      if (comboBox.getValue != null) {
        comboBox.setDisable(true)
        val newMorphism = comboBox.getValue
        addMorphism(newMorphism)
        if (newMorphism.codomain == codomain) {
          validateButton.setDisable(false)
        } else {
          validateButton.setDisable(true)
        }
        deleteButton.setDisable(false)
        deleteButton.setVisible(true)
        addButton.setDisable(true)
        addButton.setVisible(false)
        stage.sizeToScene()
      }
    })

    deleteButton.setOnAction(event => {
      gridComposition.getChildren.remove(index + 1, gridComposition.getChildren.size())
      composition = composition.dropRight(composition.size - index)
      resultPreview.setText(composition.map(_.name).reverse.mkString(" o "))
      nbOfMorphisms = index
      addButton.setVisible(true)
      addButton.setDisable(false)
      comboBox.setDisable(false)
      deleteButton.setVisible(false)
      deleteButton.setDisable(true)
      stage.sizeToScene()
    })

    val vBox = new VBox()
    vBox.setAlignment(Pos.CENTER)
    vBox.getChildren.addAll(comboBox, addButton, deleteButton)
    vBox
  }

  private def addMorphism(morphism: Morphism): Unit = {
    composition :+= morphism
    nbOfMorphisms += 1
    gridComposition.add(chooseMorphismPane(morphism.codomain, nbOfMorphisms), nbOfMorphisms % MAX_COLUMNS, nbOfMorphisms / MAX_COLUMNS)
    resultPreview.setText(composition.map(_.name).reverse.mkString(" o "))
  }

  def getResult: Morphism = {
    var result = composition.head
    for (morphism <- composition.tail) {
      result = morphism o result
    }
    result
  }
}
