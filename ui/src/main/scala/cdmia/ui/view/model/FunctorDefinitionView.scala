package cdmia.ui.view.model

import cdmia.core.categorytheory.{Category, Object}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.Morphism
import cdmia.ui.view.window.{CompositionDefinitionWindow, MainWindow}
import javafx.collections.FXCollections
import javafx.geometry.Pos
import javafx.scene
import javafx.scene.control.{Alert, Button, ComboBox, ListCell, ScrollPane}
import javafx.scene.layout.{HBox, Pane, Region, VBox}
import javafx.scene.text.Text

import scala.jdk.CollectionConverters.*

/**
 * Produce the pane allowing to define the object and morphism transformations of the functor.
 *
 * @param source the [[Category]] domain of the functor.
 * @param destination the [[Category]] codomain of the functor.
 */
class FunctorDefinitionView(val source: Category, val destination: Category) {

  val vBox = new VBox()

  // Object transformations
  private val comboBoxObjectTransformations: Map[Object, ComboBox[Object]] = source.objects.map(o => {
    val comboBox = new ComboBox[Object]()
    comboBox.setPromptText("--Select an object--")
    comboBox.setItems(FXCollections.observableList(destination.objects.toList.asJava))
    o -> comboBox
  }).toMap
  private val vBoxObjectTransformations = new VBox()
  private val objectTransformationsTitle = new Text("Object transformations")
  objectTransformationsTitle.setFont(MainWindow.TITLE_FONT)
  vBoxObjectTransformations.getChildren.add(objectTransformationsTitle)
  for (obj <- source.objects) {
    vBoxObjectTransformations.getChildren.add(getObjectTransformationPane(obj))
  }
  private val validateObjectTransformations = new Button("Validate object transformations")
  private val modifyObjectTransformations = new Button("Modify object transformations")
  modifyObjectTransformations.setDisable(true)

  // Morphism transformations
  private var comboBoxMorphismTransformations: Map[Morphism, ComboBox[Morphism]] = _
  private val vBoxMorphismTransformations = new VBox()
  private val morphismTransformationsTitle = new Text("Morphism transformations")
  morphismTransformationsTitle.setFont(MainWindow.TITLE_FONT)
  vBoxMorphismTransformations.getChildren.add(morphismTransformationsTitle)
  vBoxMorphismTransformations.getChildren.add(new Text("Validate the object transformations to select the morphism transformations."))

  private val hBoxTransformations = new HBox()
  hBoxTransformations.setAlignment(Pos.CENTER)
  hBoxTransformations.getChildren.addAll(new VBox(new ScrollPane(vBoxObjectTransformations), validateObjectTransformations, modifyObjectTransformations),
    new ScrollPane(vBoxMorphismTransformations))

  validateObjectTransformations.setOnAction(event => {
    if (comboBoxObjectTransformations.values.forall(_.getValue != null)) {
      val morphismsWithoutSolution = source.morphisms.filter(m => {
        val transformedDomain = comboBoxObjectTransformations(m.domain).getValue
        val transformedCodomain = comboBoxObjectTransformations(m.codomain).getValue
        !destination.existsMorphism(transformedDomain, transformedCodomain)
      })
      if (morphismsWithoutSolution.isEmpty) {
        vBoxMorphismTransformations.getChildren.remove(1)
        vBoxMorphismTransformations.getChildren.add(new ScrollPane(getMorphismTransformationsPane))
        for ((_, comboBox) <- comboBoxObjectTransformations) {
          comboBox.setDisable(true)
        }
        validateObjectTransformations.setDisable(true)
        modifyObjectTransformations.setDisable(false)
      } else {
        val alert = new Alert(Alert.AlertType.ERROR)
        alert.setContentText(s"The following morphisms cannot be transformed given the object transformations:\n\t-${morphismsWithoutSolution.mkString("\n\t-")}")
        alert.setHeaderText(null)
        alert.getDialogPane.setMinHeight(Region.USE_PREF_SIZE)
        alert.show()
      }
    } else {
      // TODO: adapt size to scale
      val alert = new Alert(Alert.AlertType.ERROR)
      alert.setContentText("A transformation must have been selected for all objects.")
      alert.setHeaderText(null)
      alert.getDialogPane.setMinHeight(Region.USE_PREF_SIZE)
      alert.show()
    }
  })

  modifyObjectTransformations.setOnAction(event => {
    vBoxMorphismTransformations.getChildren.remove(1, vBoxMorphismTransformations.getChildren.size)
    vBoxMorphismTransformations.getChildren.add(new Text("Validate the object transformations to select the morphism transformations."))
    validateObjectTransformations.setDisable(false)
    modifyObjectTransformations.setDisable(true)
    validateButton.setDisable(true)
    for ((_, comboBox) <- comboBoxObjectTransformations) {
      comboBox.setDisable(false)
    }
  })

  val validateButton = new Button("Create migration")
  validateButton.setDisable(true)

  vBox.setAlignment(Pos.CENTER)
  vBox.getChildren.addAll(hBoxTransformations)

  /**
   * Return the functor defined in this view.
   */
  def getFunctor: Functor = {
    new Functor(
      s"Migration ${source.name} -> ${destination.name}",
      source,
      destination,
      comboBoxObjectTransformations.map((obj, comboBox) => obj ~> comboBox.getValue),
      comboBoxMorphismTransformations.map((morphism, comboBox) => morphism ~> comboBox.getValue)
    )
  }

  private def getObjectTransformationPane(domain: Object): HBox = {
    val otHBox = new HBox()
    val comboBox = comboBoxObjectTransformations(domain)
    // Custom cell factory to scale text
    comboBox.setCellFactory(value => {
      new ListCell[Object]() {
        override def updateItem(item: Object, empty: Boolean): Unit = {
          super.updateItem(item, empty)
          if (item != null && !empty) {
            this.getListView.getTransforms.setAll(MainWindow.scale)
            setText(item.name)
          } else {
            setText(null)
          }
        }
      }
    })
    comboBox.setButtonCell(comboBox.getCellFactory.call(null))
    otHBox.getChildren.addAll(new Text(domain.name), comboBox)
    otHBox.setAlignment(Pos.CENTER_RIGHT)
    otHBox.setSpacing(20)
    otHBox
  }

  private def getMorphismTransformationsPane: VBox = {
    comboBoxMorphismTransformations = source.morphisms.map(m => {
      val transformedDomain = comboBoxObjectTransformations(m.domain).getValue
      val transformedCodomain = comboBoxObjectTransformations(m.codomain).getValue
      val comboBox = new ComboBox[Morphism]()
      comboBox.setPromptText("--Select a morphism--")
      comboBox.getItems.addAll(destination.getMorphisms(transformedDomain, transformedCodomain).asJava)
      comboBox.setOnAction(event => {
        if (comboBoxMorphismTransformations.forall((_, cb) => cb.getValue != null)) {
          validateButton.setDisable(false)
        } else {
          validateButton.setDisable(true)
        }
      })
      m -> comboBox
    }).toMap
    val maxWidthCb = comboBoxMorphismTransformations.values.map(_.getWidth).max
    comboBoxMorphismTransformations.values.foreach(cb => cb.resize(maxWidthCb, cb.getHeight))
    val vBox = new VBox()
    for (morphism <- source.morphisms) {
      vBox.getChildren.add(getMorphismTransformationPane(morphism, comboBoxObjectTransformations(morphism.domain).getValue, comboBoxObjectTransformations(morphism.codomain).getValue))
    }
    vBox
  }

  private def getMorphismTransformationPane(domain: Morphism, sourceObject: Object, destinationObject: Object): HBox = {
    val mtHBox = new HBox()
    val comboBox = comboBoxMorphismTransformations(domain)
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
    val button = new Button("Define composition transformation")
    button.setOnAction(event => {
      val compositionDefinitionWindow = new CompositionDefinitionWindow(destination, sourceObject, destinationObject)
      compositionDefinitionWindow.stage.setOnHiding(event => {
        if (compositionDefinitionWindow.compositionIsDefined) {
          val composition = compositionDefinitionWindow.getResult
          comboBox.getItems.add(composition)
          comboBox.getSelectionModel.select(composition)
        }
      })
    })
    mtHBox.getChildren.addAll(new Text(s"${domain.name}: ${domain.domain.name} -> ${domain.codomain.name}"), comboBox, button)
    mtHBox.setAlignment(Pos.CENTER_RIGHT)
    mtHBox.setSpacing(20)
    mtHBox
  }
}
