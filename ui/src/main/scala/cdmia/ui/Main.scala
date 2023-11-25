package cdmia.ui

import cdmia.ui.entity.{SchemaInitializer, TemplateMigrationInitializer}
import cdmia.ui.view.window.{GlobalModelsViewWindow, MainWindow}
import javafx.application.Application
import javafx.stage.{Stage, StageStyle}

object Main {
  def main(args: Array[String]) = {
    Application.launch(classOf[MainApplication], args: _*)
  }
}

class MainApplication extends Application {
  override def start(stage: Stage): Unit = {
    MainWindow.stage = new Stage(StageStyle.DECORATED)
    new SchemaInitializer()
    new TemplateMigrationInitializer()
    new GlobalModelsViewWindow()
  }
}
