package cdmia.ui.view.window

import javafx.geometry.Insets
import javafx.scene.Group
import javafx.scene.control.{Label, ListCell, ListView}
import javafx.scene.layout.{Background, BackgroundFill, CornerRadii, Pane}
import javafx.scene.paint.Color
import javafx.scene.shape.Line
import javafx.scene.text.{Font, FontWeight}
import javafx.scene.transform.Scale
import javafx.stage.{Screen, Stage}

object MainWindow {
  val DEFAULT_FONT: Font = Font.font(10)
  val HOVERED_TEXT_FONT: Font = Font.font(12)
  val TITLE_FONT: Font = Font.font(null, FontWeight.BOLD, 14)
  val CATEGORY_FONT: Font = Font.font(null, FontWeight.BOLD, 16)
  val TOTAL_CATEGORY_WIDTH: Double = 775
  val CATEGORY_WIDTH: Double = 600
  val CATEGORY_HEIGHT: Double = 350
  val FUNCTOR_SEPARATOR_HEIGHT: Double = 50

  private val screenBounds = Screen.getPrimary.getBounds
  val scale = new Scale(screenBounds.getWidth / 1920, screenBounds.getHeight / 1080, 0, 0)

  var stage: Stage = null

  def drawArrowLine (startX: Double, startY: Double, endX: Double, endY: Double, pane: Pane, label: String = ""): Unit = {
    // get the slope of the line and find its angle
    val slope: Double = (startY - endY) / (startX - endX)
    val lineAngle: Double = Math.atan(slope)

    val arrowAngle: Double = if (startX >= endX) Math.toRadians(45) else -Math.toRadians(225)

    val line: Line = new Line(startX, startY, endX, endY)

    val lineLength: Double = Math.sqrt(Math.pow(startX - endX, 2) + Math.pow(startY - endY, 2))
    val arrowLength: Double = 15

    // create the arrow legs
    val arrow1: Line = new Line()
    arrow1.setStartX(line.getEndX)
    arrow1.setStartY(line.getEndY)
    arrow1.setEndX(line.getEndX + arrowLength * Math.cos(lineAngle - arrowAngle))
    arrow1.setEndY(line.getEndY + arrowLength * Math.sin(lineAngle - arrowAngle))

    val arrow2: Line = new Line()
    arrow2.setStartX(line.getEndX)
    arrow2.setStartY(line.getEndY)
    arrow2.setEndX(line.getEndX + arrowLength * Math.cos(lineAngle + arrowAngle))
    arrow2.setEndY(line.getEndY + arrowLength * Math.sin(lineAngle + arrowAngle))

    // Arrow style
    val strokeWidth: Double = 3
    val lineColor: Color = Color.DARKBLUE
    line.setStrokeWidth(strokeWidth)
    line.setStroke(lineColor)
    arrow1.setStrokeWidth(strokeWidth)
    arrow1.setStroke(lineColor)
    arrow2.setStrokeWidth(strokeWidth)
    arrow2.setStroke(lineColor)

    pane.getChildren.addAll(line, arrow1, arrow2)

    if (label.nonEmpty) {
      val text = new Label(label)
      text.setFont(TITLE_FONT)
      text.setTextFill(lineColor)
      text.setBackground(new Background(new BackgroundFill(Color.WHITE, CornerRadii.EMPTY, Insets.EMPTY)))
      text.setTranslateX((Math.min(startX, endX) + Math.abs(startX - endX) / 2) - 60)
      text.setTranslateY((Math.min(startY, endY) + Math.abs(startY - endY) / 2) - 7)
      pane.getChildren.add(new Group(text))
    }
  }

  def resizeListView[T](listView: ListView[T], size: Double): Unit = {
    listView.setPrefWidth(size)
    listView.setCellFactory(item => {
      new ListCell[T]() {
        prefWidthProperty().bind(listView.widthProperty().subtract(15))
        setMaxWidth(getPrefWidth)
        setWidth(getPrefWidth)

        override def updateItem(item: T, empty: Boolean): Unit = {
          super.updateItem(item, empty)
          if (item != null && !empty) {
            this.setWrapText(true)
            setText(item.toString)
          } else {
            setText(null)
          }
        }
      }
    })
  }
}
