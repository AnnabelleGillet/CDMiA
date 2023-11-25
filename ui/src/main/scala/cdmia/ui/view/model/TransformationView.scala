package cdmia.ui.view.model

import cdmia.datawrapper.modeltransformation.SchemaModelTransformation
import cdmia.ui.view.window.MainWindow
import javafx.geometry.Pos
import javafx.scene.layout
import javafx.scene.layout.{HBox, Pane, VBox}

/**
 * Produce the view for a transformation, with the schema, the source model, the destination model, and the
 * preservations and creations implied by the transformation.
 * 
 * @param transformation the [[SchemaModelTransformation]] to display.
 */
class TransformationView(val transformation: SchemaModelTransformation) {

  // Schema, model source and model destination category views
  private val schemaView: CategoryView = new CategoryView(transformation.source.category)
  private val destinationModelView: CategoryView = new CategoryView(transformation.functor.codomain)
  private val sourceModelView: CategoryView = new CategoryView(transformation.source.model.category)

  private val preservationAndCreationView = new PreservationsAndCreationsView(schemaView, destinationModelView, transformation.preservationOutputs, transformation.creationOutputs, transformation.functor)
  private val preservationAndCreationPane = preservationAndCreationView.preservationAndCreationPane

  private val hBoxModels: HBox = new HBox()
  hBoxModels.setAlignment(Pos.CENTER)
  hBoxModels.getChildren.addAll(sourceModelView.borderPane, destinationModelView.borderPane)
  val vBox: VBox = new VBox()
  vBox.setAlignment(Pos.CENTER)
  vBox.getChildren.add(hBoxModels)

  private val functorArrow = new Pane()
  functorArrow.setMinWidth(MainWindow.TOTAL_CATEGORY_WIDTH * 2)
  functorArrow.setMinHeight(MainWindow.FUNCTOR_SEPARATOR_HEIGHT)
  // Functor to source model
  MainWindow.drawArrowLine(MainWindow.CATEGORY_WIDTH / 2, MainWindow.FUNCTOR_SEPARATOR_HEIGHT, MainWindow.CATEGORY_WIDTH / 2, 10, functorArrow, "Source model")
  // Functor to destination model
  MainWindow.drawArrowLine(MainWindow.TOTAL_CATEGORY_WIDTH / 2, MainWindow.FUNCTOR_SEPARATOR_HEIGHT, MainWindow.TOTAL_CATEGORY_WIDTH + (MainWindow.CATEGORY_WIDTH / 2), 10, functorArrow, "Destination model")
  vBox.getChildren.add(functorArrow)
  private val hBoxResultTransformation: HBox = new HBox()
  hBoxResultTransformation.setAlignment(Pos.CENTER)
  hBoxResultTransformation.getChildren.addAll(schemaView.borderPane, preservationAndCreationPane)

  vBox.getChildren.add(hBoxResultTransformation)

  // Add highlighting between schema and source model
  FunctorView.addCorrespondance(transformation.source.functorTowardsModel, schemaView, sourceModelView)
  // Add highlighting between schema and destination model
  FunctorView.addCorrespondance(transformation.functor, schemaView, destinationModelView)
  // If the transformation follows a template, add highlighting also between models
  if (transformation.template.isDefined) {
    FunctorView.addCorrespondance(transformation.template.get.functor, sourceModelView, destinationModelView)
  }
  
  def init(): Unit = {
    schemaView.init()
    sourceModelView.init()
    destinationModelView.init()
  }
  
  def update(): Unit = {
    schemaView.update()
    sourceModelView.update()
    destinationModelView.update()
  }
}
