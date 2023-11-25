package cdmia.ui.view.model

import cdmia.core.categorytheory.{Category, Object}
import cdmia.core.categorytheory.morphism.{IdentityMorphism, Morphism, MorphismComposition}
import cdmia.ui.view.graphplacement.CategoryPlacementStrategy
import cdmia.ui.view.window.MainWindow
import com.brunomnsilva.smartgraph.graph.{Digraph, DigraphEdgeList}
import com.brunomnsilva.smartgraph.graphview.{SmartGraphEdgeCurve, SmartGraphEdgeLine, SmartGraphPanel, SmartGraphProperties}
import javafx.event.EventHandler
import javafx.scene.control.ScrollPane
import javafx.scene.input.MouseEvent
import javafx.scene.layout
import javafx.scene.layout.*
import javafx.scene.paint.Color
import javafx.scene.text.Text

import java.io.File

/**
 * Produce the view for a category.
 *
 * @param category the [[Category]] to display.
 */
class CategoryView(val category: Category) {
  // VBox for object and morphism names
  private val vBoxView = new VBox()
  vBoxView.setSpacing(1)

  val graph: Digraph[ObjectView, MorphismView] = new DigraphEdgeList[ObjectView, MorphismView]()

  // Objects of the category
  private val objectsText = new Text("Objects")
  objectsText.setFont(MainWindow.TITLE_FONT)
  vBoxView.getChildren.add(objectsText)
  protected case class TextAndView[T](text: Text, view: T)
  val objects: Map[Object, TextAndView[ObjectView]] = initializeObjects

  // Morphisms of the category
  private val morphismsText = new Text("Morphisms")
  morphismsText.setFont(MainWindow.TITLE_FONT)
  vBoxView.getChildren.add(morphismsText)
  val morphisms: Map[Morphism, TextAndView[MorphismView]] = initializeMorphisms

  // Patterns of the category
  var index = 0
  if (category.limits.nonEmpty || category.colimits.nonEmpty) {
    val patternsText = new Text("Patterns")
    patternsText.setFont(MainWindow.TITLE_FONT)
    vBoxView.getChildren.add(index, patternsText)
    index += 1
    for (pattern <- category.limits.concat(category.colimits)) {
      val item = new Text(pattern.name)
      item.setOnMouseEntered((event: MouseEvent) => {
        highlightAll(pattern.getObjects, pattern.getMorphisms)
      })
      item.setOnMouseExited((event: MouseEvent) => {
        removeHighlightOnAll(pattern.getObjects, pattern.getMorphisms)
      })
      vBoxView.getChildren.add(index, item)
      index += 1
    }
  }

  // Generate the graph view
  private val strategy = new CategoryPlacementStrategy()
  private val properties = new SmartGraphProperties(getClass.getClassLoader.getResourceAsStream("smartgraph.properties"))
  private val css = getClass.getClassLoader.getResource(s"css${File.separator}smartgraph.css")
  private val graphView: SmartGraphPanel[ObjectView, MorphismView] = new SmartGraphPanel[ObjectView, MorphismView](graph, properties, strategy, css.toURI)

  // Isomorphisms of the category
  if (category.isomorphisms.nonEmpty) {
    val specificMorphismsText = new Text("Specific morphisms")
    specificMorphismsText.setFont(MainWindow.TITLE_FONT)
    vBoxView.getChildren.add(index, specificMorphismsText)
    index += 1
    for (isomorphism <- category.isomorphisms) {
      val item = new Text(s"Isomorphism: ${isomorphism.morphism.name}")
      item.setOnMouseEntered((event: MouseEvent) => {
        highlightMorphism(isomorphism.morphism)
        highlightMorphism(isomorphism.inverse)
      })
      item.setOnMouseExited((event: MouseEvent) => {
        removeHighlightOnMorphism(isomorphism.morphism)
        removeHighlightOnMorphism(isomorphism.inverse)
      })

      // Style of isomorphism
      val edgeIsomorphism = graphView.getStylableEdge(new MorphismView(isomorphism.morphism))
      edgeIsomorphism match
        case line: SmartGraphEdgeLine[_, _] => if (line.getAttachedArrow != null) {
          line.getAttachedArrow.addStyleClass("isomorphism")
          line.getAttachedLabel.addStyleClass("isomorphism")
        }
        case curve: SmartGraphEdgeCurve[_, _] => if (curve.getAttachedArrow != null) {
          curve.getAttachedArrow.addStyleClass("isomorphism")
          curve.getAttachedLabel.addStyleClass("isomorphism")
        }
      edgeIsomorphism.addStyleClass("isomorphism")

      // Style of inverse
      val edgeInverse = graphView.getStylableEdge(new MorphismView(isomorphism.inverse))
      edgeInverse match
        case line: SmartGraphEdgeLine[_, _] => if (line.getAttachedArrow != null) {
          line.getAttachedArrow.addStyleClass("isomorphism")
          line.getAttachedLabel.addStyleClass("isomorphism")
          line.getAttachedArrow.addStyleClass("inverse")
          line.getAttachedLabel.addStyleClass("inverse")
        }
        case curve: SmartGraphEdgeCurve[_, _] => if (curve.getAttachedArrow != null) {
          curve.getAttachedArrow.addStyleClass("isomorphism")
          curve.getAttachedLabel.addStyleClass("isomorphism")
          curve.getAttachedArrow.addStyleClass("inverse")
          curve.getAttachedLabel.addStyleClass("inverse")
        }
      edgeInverse.addStyleClass("isomorphism")
      edgeInverse.addStyleClass("inverse")


      vBoxView.getChildren.add(index, item)
      index += 1
    }
  }

  // TODO morphism equalities

  // Generate the pane for the category
  val borderPane = new BorderPane()
  private val categoryName = new Text(category.name)
  categoryName.setFont(MainWindow.CATEGORY_FONT)
  borderPane.setTop({
    val hBox = new HBox()
    hBox.getChildren.add(categoryName)
    hBox
  })
  graphView.setPrefWidth(MainWindow.CATEGORY_WIDTH)
  graphView.setPrefHeight(MainWindow.CATEGORY_HEIGHT)
  graphView.resize(MainWindow.CATEGORY_WIDTH, MainWindow.CATEGORY_HEIGHT)
  borderPane.setCenter(graphView)
  vBoxView.setPrefWidth(MainWindow.TOTAL_CATEGORY_WIDTH - MainWindow.CATEGORY_WIDTH)
  private val scrollPane = new ScrollPane(vBoxView)
  borderPane.setRight(scrollPane)
  graphView.setBorder(new Border(new BorderStroke(Color.BLACK, BorderStrokeStyle.SOLID, CornerRadii.EMPTY, BorderWidths.DEFAULT)))

  def init(): Unit = {
    graphView.init()
    borderPane.setMaxHeight(graphView.getHeight)
    // Hide identity morphisms
    for (identity <- category.identityMorphisms) {
      val edge = graphView.getStylableEdge(new MorphismView(identity))
      edge match
        case line: SmartGraphEdgeLine[_, _] => if (line.getAttachedArrow != null) {
          line.getAttachedArrow.addStyleClass("hidden")
          line.getAttachedLabel.addStyleClass("hidden")
        }
        case curve: SmartGraphEdgeCurve[_, _] => if (curve.getAttachedArrow != null) {
          curve.getAttachedArrow.addStyleClass("hidden")
          curve.getAttachedLabel.addStyleClass("hidden")
        }
      edge.addStyleClass("hidden")
    }
  }
  
  def update(): Unit = {
    graphView.update()
  }
  
  /**
   * Highlights the given object in the category view.
   *
   * @param obj the [[Object]] to highlight.
   */
  def highlightObject(obj: Object): Unit = {
    val TextAndView(text, view) = objects(obj)
    graphView.getStylableVertex(view).addStyleClass("hover")
    text.setFont(MainWindow.HOVERED_TEXT_FONT)
  }

  /**
   * Remove the highlight on the given object in the category view.
   *
   * @param obj the [[Object]] for which to remove the highlight.
   */
  def removeHighlightOnObject(obj: Object): Unit = {
    val TextAndView(text, view) = objects(obj)
    graphView.getStylableVertex(view).removeStyleClass("hover")
    text.setFont(MainWindow.DEFAULT_FONT)
  }

  /**
   * Highlights the given morphism in the category view.
   *
   * @param morphism the [[Morphism]] to highlight.
   */
  def highlightMorphism(morphism: Morphism): Unit = {
    val TextAndView(text, view) = morphisms(morphism)
    val edge = graphView.getStylableEdge(view)
    edge match
      case line: SmartGraphEdgeLine[_, _] => if (line.getAttachedArrow != null) {
        line.getAttachedArrow.addStyleClass("hover")
        line.getAttachedLabel.addStyleClass("hover")
      }
      case curve: SmartGraphEdgeCurve[_, _] => if (curve.getAttachedArrow != null) {
        curve.getAttachedArrow.addStyleClass("hover")
        curve.getAttachedLabel.addStyleClass("hover")
      }
    edge.addStyleClass("hover")
    text.setFont(MainWindow.HOVERED_TEXT_FONT)
  }

  /**
   * Remove the highlight on the given morphism in the category view.
   *
   * @param morphism the [[Morphism]] for which to remove the highlight.
   */
  def removeHighlightOnMorphism(morphism: Morphism): Unit = {
    val TextAndView(text, view) = morphisms(morphism)
    val edge = graphView.getStylableEdge(view)
    edge match
      case line: SmartGraphEdgeLine[_, _] => if (line.getAttachedArrow != null) {
        line.getAttachedArrow.removeStyleClass("hover")
        line.getAttachedLabel.removeStyleClass("hover")
      }
      case curve: SmartGraphEdgeCurve[_, _] => if (curve.getAttachedArrow != null) {
        curve.getAttachedArrow.removeStyleClass("hover")
        curve.getAttachedLabel.removeStyleClass("hover")
      }
    edge.removeStyleClass("hover")
    text.setFont(MainWindow.DEFAULT_FONT)
  }

  /**
   * Highlights the given morphism in the category view.
   *
   * @param morphism the [[Morphism]] to highlight.
   */
  def highlightIdentityMorphism(morphism: Morphism, bad: Boolean = false): Unit = {
    val view = new MorphismView(morphism)
    val edge = graphView.getStylableEdge(view)
    edge match
      case line: SmartGraphEdgeLine[_, _] => if (line.getAttachedArrow != null) {
        line.getAttachedArrow.addStyleClass("hover")
        line.getAttachedLabel.addStyleClass("hover")
        if (bad) {
          line.getAttachedArrow.addStyleClass("bad")
          line.getAttachedLabel.addStyleClass("bad")
        }
      }
      case curve: SmartGraphEdgeCurve[_, _] => if (curve.getAttachedArrow != null) {
        curve.getAttachedArrow.addStyleClass("hover")
        curve.getAttachedLabel.addStyleClass("hover")
        if (bad) {
          curve.getAttachedArrow.addStyleClass("bad")
          curve.getAttachedLabel.addStyleClass("bad")
        }
      }
    edge.addStyleClass("hover")
    if (bad) {
      edge.addStyleClass("bad")
    }
  }

  /**
   * Remove the highlight on the given morphism in the category view.
   *
   * @param morphism the [[Morphism]] for which to remove the highlight.
   */
  def removeHighlightOnIdentityMorphism(morphism: Morphism, bad: Boolean = false): Unit = {
    val view = new MorphismView(morphism)
    val edge = graphView.getStylableEdge(view)
    edge match
      case line: SmartGraphEdgeLine[_, _] => if (line.getAttachedArrow != null) {
        line.getAttachedArrow.removeStyleClass("hover")
        line.getAttachedLabel.removeStyleClass("hover")
        if (bad) {
          line.getAttachedArrow.removeStyleClass("bad")
          line.getAttachedLabel.removeStyleClass("bad")
        }
      }
      case curve: SmartGraphEdgeCurve[_, _] => if (curve.getAttachedArrow != null) {
        curve.getAttachedArrow.removeStyleClass("hover")
        curve.getAttachedLabel.removeStyleClass("hover")
        if (bad) {
          curve.getAttachedArrow.removeStyleClass("bad")
          curve.getAttachedLabel.removeStyleClass("bad")
        }
      }
    edge.removeStyleClass("hover")
    if(bad) {
      edge.removeStyleClass("bad")
    }
  }

  /**
   * Highlights the given objects and morphisms in the category view.
   *
   * @param _objects the [[Object]]s to highlight.
   * @param _morphisms the [[Morphism]]s to highlight.
   * @param bad if the highlight is bad or not.
   */
  def highlightAll(_objects: List[Object], _morphisms: List[Morphism], bad: Boolean = false): Unit = {
    for (obj <- _objects) {
      val TextAndView(text, view) = objects(obj)
      graphView.getStylableVertex(view).addStyleClass("hover")
      if (bad) {
        graphView.getStylableVertex(view).addStyleClass("bad")
      }
      text.setFont(MainWindow.HOVERED_TEXT_FONT)
    }

    def highlightIndividualMorphism(morphism: Morphism): Unit = {
      val TextAndView(text, view) = morphisms(morphism)
      val edge = graphView.getStylableEdge(view)
      edge match
        case line: SmartGraphEdgeLine[_, _] => if (line.getAttachedArrow != null) {
          line.getAttachedArrow.addStyleClass("hover")
          line.getAttachedLabel.addStyleClass("hover")
          if (bad) {
            line.getAttachedArrow.addStyleClass("bad")
            line.getAttachedLabel.addStyleClass("bad")
          }
        }
        case curve: SmartGraphEdgeCurve[_, _] => if (curve.getAttachedArrow != null) {
          curve.getAttachedArrow.addStyleClass("hover")
          curve.getAttachedLabel.addStyleClass("hover")
          if (bad) {
            curve.getAttachedArrow.addStyleClass("bad")
            curve.getAttachedLabel.addStyleClass("bad")
          }
        }
      edge.addStyleClass("hover")
      if (bad) {
        edge.addStyleClass("bad")
      }
      text.setFont(MainWindow.HOVERED_TEXT_FONT)
    }

    for (morphism <- _morphisms) {
      morphism match
        case composition: MorphismComposition => composition.chainOfMorphisms.foreach(highlightIndividualMorphism)
        case identityMorphism: IdentityMorphism => highlightIdentityMorphism(identityMorphism, bad)
        case _ => highlightIndividualMorphism(morphism)
    }
  }

  /**
   * Remove the highlight on the given objects and morphisms in the category view.
   *
   * @param _objects the [[Object]]s for which to remove the highlight.
   * @param _morphisms the [[Morphism]]s for which to remove the highlight.
   * @param bad if the highlight is bad or not.
   */
  def removeHighlightOnAll(_objects: List[Object], _morphisms: List[Morphism], bad: Boolean = false): Unit = {
    for (obj <- _objects) {
      val TextAndView(text, view) = objects(obj)
      graphView.getStylableVertex(view).removeStyleClass("hover")
      graphView.getStylableVertex(view).removeStyleClass("bad")
      text.setFont(MainWindow.DEFAULT_FONT)
    }

    def removeHighlightIndividualMorphism(morphism: Morphism): Unit = {
      val TextAndView(text, view) = morphisms(morphism)
      val edge = graphView.getStylableEdge(view)
      edge match
        case line: SmartGraphEdgeLine[_, _] => if (line.getAttachedArrow != null) {
          line.getAttachedArrow.removeStyleClass("hover")
          line.getAttachedLabel.removeStyleClass("hover")
          if (bad) {
            line.getAttachedArrow.removeStyleClass("bad")
            line.getAttachedLabel.removeStyleClass("bad")
          }
        }
        case curve: SmartGraphEdgeCurve[_, _] => if (curve.getAttachedArrow != null) {
          curve.getAttachedArrow.removeStyleClass("hover")
          curve.getAttachedLabel.removeStyleClass("hover")
          if (bad) {
            curve.getAttachedArrow.removeStyleClass("bad")
            curve.getAttachedLabel.removeStyleClass("bad")
          }
        }
      edge.removeStyleClass("hover")
      if (bad) {
        edge.removeStyleClass("bad")
      }
      text.setFont(MainWindow.DEFAULT_FONT)
    }

    for (morphism <- _morphisms) {
      morphism match
        case composition: MorphismComposition => composition.chainOfMorphisms.foreach(removeHighlightIndividualMorphism)
        case identityMorphism: IdentityMorphism => removeHighlightOnIdentityMorphism(identityMorphism, bad)
        case _ => removeHighlightIndividualMorphism(morphism)
    }
  }

  def addMorphism(morphism: Morphism): Unit = {
    val morphismView = new MorphismView(morphism)
    graph.insertEdge(objects(morphism.domain).view, objects(morphism.codomain).view, morphismView)
  }

  private def initializeObjects: Map[Object, TextAndView[ObjectView]] = {
    (for (obj <- category.objects) yield {
      val objectView = new ObjectView(obj)
      graph.insertVertex(objectView)
      val item = new Text(obj.name)

      item.setOnMouseEntered((event: MouseEvent) => {
        highlightObject(obj)
      })
      item.setOnMouseExited((event: MouseEvent) => {
        removeHighlightOnObject(obj)
      })

      vBoxView.getChildren.add(item)
      obj -> TextAndView(item, objectView)
    }).toMap
  }

  private def initializeMorphisms: Map[Morphism, TextAndView[MorphismView]] = {
    for (morphism <- category.identityMorphisms) {
      val edge = graph.insertEdge(objects(morphism.domain).view, objects(morphism.codomain).view, new MorphismView(morphism))
    }

    (for (morphism <- category.morphisms) yield {
      val morphismView = new MorphismView(morphism)
      val item = new Text(morphism.name)
      item.setOnMouseEntered((event: MouseEvent) => {
        highlightMorphism(morphism)
      })
      item.setOnMouseExited((event: MouseEvent) => {
        removeHighlightOnMorphism(morphism)
      })
      vBoxView.getChildren.add(item)
      graph.insertEdge(objects(morphism.domain).view, objects(morphism.codomain).view, morphismView)
      morphism -> TextAndView(item, morphismView)
    }).toMap
  }
}
