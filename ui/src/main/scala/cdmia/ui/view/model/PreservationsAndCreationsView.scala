package cdmia.ui.view.model

import cdmia.datawrapper.modeltransformation.{Creation, CreationOutput, Preservation, PreservationOutput}
import cdmia.core.categorytheory.functor.Functor
import cdmia.ui.view.window.MainWindow
import javafx.geometry.Orientation
import javafx.scene.control.{ScrollPane, Separator}
import javafx.scene.layout.{HBox, VBox}
import javafx.scene.text.Text

/**
 * Display the valid and invalid preservations and creations for a given transformation.
 *
 * @param sourceView the source [[Category]] of the transformation.
 * @param destinationView the destination [[Category]] of the transformation.
 * @param preservations the [[PreservationOutput]]s of the transformation.
 * @param creations the [[CreationOutput]]s of the transformation.
 * @param functor the [[Functor]] specifying the transformation.
 */
class PreservationsAndCreationsView(val sourceView: CategoryView, val destinationView: CategoryView,
                                    val preservations: Map[Preservation, PreservationOutput], val creations: Map[Creation, List[CreationOutput]],
                                    val functor: Functor) {

  val preservationAndCreationPane = new HBox()
  preservationAndCreationPane.resize(MainWindow.TOTAL_CATEGORY_WIDTH, MainWindow.CATEGORY_HEIGHT)

  // Preservation of constraints and specificities
  private val vBoxPreservations: VBox = new VBox()
  vBoxPreservations.setPrefWidth(MainWindow.TOTAL_CATEGORY_WIDTH / 2)
  vBoxPreservations.resize(MainWindow.TOTAL_CATEGORY_WIDTH / 2, MainWindow.CATEGORY_HEIGHT)
  vBoxPreservations.setSpacing(2)
  private val preservationsTitle = new Text("Preservations")
  preservationsTitle.setFont(MainWindow.TITLE_FONT)
  vBoxPreservations.getChildren.add(preservationsTitle)
  for ((preservation, output) <- preservations) {
    val text = new Text(s"\t- ${output.message}\n")
    text.wrappingWidthProperty().bind(vBoxPreservations.widthProperty().subtract(15))
    text.getStyleClass.add("text")
    if (output.preserved) {
      text.getStyleClass.add("valid")
    } else {
      text.getStyleClass.add("error")
    }
    text.setOnMouseEntered(event => {
      destinationView.highlightAll(preservation.concernedObjects.map(functor.getDestinationObject),
        preservation.concernedMorphisms.map(functor.getDestinationMorphism), !output.preserved)
      sourceView.highlightAll(preservation.concernedObjects, preservation.concernedMorphisms, !output.preserved)
    })
    text.setOnMouseExited(event => {
      destinationView.removeHighlightOnAll(preservation.concernedObjects.map(functor.getDestinationObject),
        preservation.concernedMorphisms.map(functor.getDestinationMorphism), !output.preserved)
      sourceView.removeHighlightOnAll(preservation.concernedObjects, preservation.concernedMorphisms, !output.preserved)
    })
    vBoxPreservations.getChildren.add(text)
  }
  
  // Creation of constraints and specificities
  private val vBoxCreations: VBox = new VBox()
  vBoxCreations.setPrefWidth(MainWindow.TOTAL_CATEGORY_WIDTH / 2)
  vBoxCreations.resize(MainWindow.TOTAL_CATEGORY_WIDTH / 2, MainWindow.CATEGORY_HEIGHT)
  vBoxCreations.setSpacing(2)
  private val creationsTitle = new Text("Creations")
  creationsTitle.setFont(MainWindow.TITLE_FONT)
  vBoxCreations.getChildren.add(creationsTitle)
  for ((creation, outputs) <- creations) {
    for (output <- outputs) {
      val text = new Text(s"\t- ${output.message}\n")
      text.wrappingWidthProperty().bind(vBoxCreations.widthProperty().subtract(15))
      text.getStyleClass.add("text")
      if (output.preserved) {
        text.getStyleClass.add("valid")
      } else {
        text.getStyleClass.add("error")
      }
      text.setOnMouseEntered(event => {
        sourceView.highlightAll(output.concernedObjects.filter(_.isInCategory(sourceView.category)), output.concernedMorphisms.filter(_.isInCategory(sourceView.category)), !output.preserved)
        destinationView.highlightAll(creation.concernedObjects, creation.concernedMorphisms, !output.preserved)
      })
      text.setOnMouseExited(event => {
        sourceView.removeHighlightOnAll(output.concernedObjects.filter(_.isInCategory(sourceView.category)), output.concernedMorphisms.filter(_.isInCategory(sourceView.category)), !output.preserved)
        destinationView.removeHighlightOnAll(creation.concernedObjects, creation.concernedMorphisms, !output.preserved)
      })
      vBoxCreations.getChildren.add(text)
    }
  }

  private val separator = new Separator()
  separator.setOrientation(Orientation.VERTICAL)

  private val scrollPanePreservations = new ScrollPane(vBoxPreservations)
  scrollPanePreservations.setHbarPolicy(ScrollPane.ScrollBarPolicy.AS_NEEDED)
  scrollPanePreservations.setVbarPolicy(ScrollPane.ScrollBarPolicy.AS_NEEDED)
  scrollPanePreservations.setMaxWidth(MainWindow.TOTAL_CATEGORY_WIDTH / 2)
  scrollPanePreservations.setMaxHeight(MainWindow.CATEGORY_HEIGHT)
  scrollPanePreservations.resize(MainWindow.TOTAL_CATEGORY_WIDTH / 2, MainWindow.CATEGORY_HEIGHT)
  scrollPanePreservations.setFitToWidth(true)

  private val scrollPaneCreations = new ScrollPane(vBoxCreations)
  scrollPaneCreations.setHbarPolicy(ScrollPane.ScrollBarPolicy.AS_NEEDED)
  scrollPaneCreations.setVbarPolicy(ScrollPane.ScrollBarPolicy.AS_NEEDED)
  scrollPaneCreations.setMaxWidth(MainWindow.TOTAL_CATEGORY_WIDTH / 2)
  scrollPaneCreations.setMaxHeight(MainWindow.CATEGORY_HEIGHT)
  scrollPaneCreations.resize(MainWindow.TOTAL_CATEGORY_WIDTH / 2, MainWindow.CATEGORY_HEIGHT)
  scrollPaneCreations.setFitToWidth(true)
  
  preservationAndCreationPane.getChildren.addAll(scrollPanePreservations, separator, scrollPaneCreations)
}
