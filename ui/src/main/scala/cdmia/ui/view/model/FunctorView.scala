package cdmia.ui.view.model

import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.{IdentityMorphism, MorphismComposition}
import cdmia.ui.view.window.MainWindow
import javafx.geometry.Pos
import javafx.scene.input.MouseEvent
import javafx.scene.layout.{Pane, VBox}
import javafx.scene.text.{Font, FontWeight}

/**
 * Produce the view for a functor. Both the categories source and destination are represented, as well as the
 * object and morphism transformations.
 *
 * @param functor the [[Functor]] to display.
 */
class FunctorView(val functor: Functor) {
  private val FUNCTOR_FONT: Font = Font.font(null, FontWeight.BOLD,30)

  // Source and destination category views
  val sourceView: CategoryView = new CategoryView(functor.domain)
  val destinationView: CategoryView = new CategoryView(functor.codomain)

  val vBox: VBox = new VBox()
  vBox.setAlignment(Pos.CENTER)
  vBox.getChildren.add(destinationView.borderPane)
  
  private val functorArrow = new Pane()
  functorArrow.setMinWidth(MainWindow.TOTAL_CATEGORY_WIDTH)
  functorArrow.setMinHeight(MainWindow.FUNCTOR_SEPARATOR_HEIGHT)
  // Functor to source model
  MainWindow.drawArrowLine(MainWindow.CATEGORY_WIDTH / 2, MainWindow.FUNCTOR_SEPARATOR_HEIGHT, MainWindow.CATEGORY_WIDTH / 2, 10, functorArrow, "Source model")
  
  vBox.getChildren.add(functorArrow)
  vBox.getChildren.add(sourceView.borderPane)

  FunctorView.addCorrespondance(functor, sourceView, destinationView)

  def init(): Unit = {
    sourceView.init()
    destinationView.init()
  }

  def update(): Unit = {
    sourceView.update()
    destinationView.update()
  }
}

object FunctorView {
  def addCorrespondance(functor: Functor, sourceView: CategoryView, destinationView: CategoryView): Unit = {
    // Source events on objects
    for (ot <- functor.objectTransformations) {
      val sourceTextAndView = sourceView.objects(ot.source)
      val previousEnteredEvent = sourceTextAndView.text.getOnMouseEntered
      sourceTextAndView.text.setOnMouseEntered((event: MouseEvent) => {
        previousEnteredEvent.handle(event)
        destinationView.highlightObject(ot.destination)
      })
      val previousExitedEvent = sourceTextAndView.text.getOnMouseExited
      sourceTextAndView.text.setOnMouseExited((event: MouseEvent) => {
        previousExitedEvent.handle(event)
        destinationView.removeHighlightOnObject(ot.destination)
      })
    }
    // Destination events on objects
    for (obj <- functor.codomain.objects) {
      val sourceObjects = functor.getSourceObjects(obj)
      if (sourceObjects.nonEmpty) {
        val destinationTextAndView = destinationView.objects(obj)
        val previousEnteredEvent = destinationTextAndView.text.getOnMouseEntered
        destinationTextAndView.text.setOnMouseEntered((event: MouseEvent) => {
          previousEnteredEvent.handle(event)
          for (sourceObject <- sourceObjects) {
            sourceView.highlightObject(sourceObject)
          }
        })
        val previousExitedEvent = destinationTextAndView.text.getOnMouseExited
        destinationTextAndView.text.setOnMouseExited((event: MouseEvent) => {
          previousExitedEvent.handle(event)
          for (sourceObject <- sourceObjects) {
            sourceView.removeHighlightOnObject(sourceObject)
          }
        })
      }
    }

    // Source events on morphisms
    for (mt <- functor.morphismTransformations) {
      val sourceTextAndView = sourceView.morphisms(mt.source)
      val previousEnteredEvent = sourceTextAndView.text.getOnMouseEntered
      val previousExitedEvent = sourceTextAndView.text.getOnMouseExited
      mt.destination match
        case identity: IdentityMorphism => {
          sourceTextAndView.text.setOnMouseEntered((event: MouseEvent) => {
            previousEnteredEvent.handle(event)
            destinationView.highlightIdentityMorphism(identity)
          })
          sourceTextAndView.text.setOnMouseExited((event: MouseEvent) => {
            previousExitedEvent.handle(event)
            destinationView.removeHighlightOnIdentityMorphism(identity)
          })
        }
        case composition: MorphismComposition => {
          sourceTextAndView.text.setOnMouseEntered((event: MouseEvent) => {
            previousEnteredEvent.handle(event)
            for (morphism <- composition.chainOfMorphisms) {
              morphism match
                case identity: IdentityMorphism => destinationView.highlightIdentityMorphism(morphism)
                case _ => destinationView.highlightMorphism(morphism)
            }
          })
          sourceTextAndView.text.setOnMouseExited((event: MouseEvent) => {
            previousExitedEvent.handle(event)
            for (morphism <- composition.chainOfMorphisms) {
              morphism match
                case identity: IdentityMorphism => destinationView.removeHighlightOnIdentityMorphism(morphism)
                case _ => destinationView.removeHighlightOnMorphism(morphism)
            }
          })
        }
        case _ => {
          sourceTextAndView.text.setOnMouseEntered((event: MouseEvent) => {
            previousEnteredEvent.handle(event)
            destinationView.highlightMorphism(mt.destination)
          })
          sourceTextAndView.text.setOnMouseExited((event: MouseEvent) => {
            previousExitedEvent.handle(event)
            destinationView.removeHighlightOnMorphism(mt.destination)
          })
        }
    }
    // Destination events on morphisms
    for (morphism <- functor.codomain.morphisms) {
      val sourceMorphisms = functor.getSourceMorphisms(morphism)
      if (sourceMorphisms.nonEmpty) {
        val destinationTextAndView = destinationView.morphisms(morphism)
        val previousEnteredEvent = destinationTextAndView.text.getOnMouseEntered
        destinationTextAndView.text.setOnMouseEntered((event: MouseEvent) => {
          previousEnteredEvent.handle(event)
          for (sourceMorphism <- sourceMorphisms) {
            sourceView.highlightMorphism(sourceMorphism)
          }
        })
        val previousExitedEvent = destinationTextAndView.text.getOnMouseExited
        destinationTextAndView.text.setOnMouseExited((event: MouseEvent) => {
          previousExitedEvent.handle(event)
          for (sourceMorphism <- sourceMorphisms) {
            sourceView.removeHighlightOnMorphism(sourceMorphism)
          }
        })
      }
    }
  }
}
