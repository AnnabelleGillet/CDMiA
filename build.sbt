ThisBuild / organization := "io.github.annabellegillet"
ThisBuild / version := "0.2.0"

ThisBuild / scalaVersion := "3.3.0"

ThisBuild / libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test

lazy val root = (project in file("."))
  .settings(
    name := "CDMiA",
    Compile / run / fork := true,
    assembly / mainClass := Some("cdmia.ui.Main"),
    assemblyJarName in assembly := s"CDMiA-${version.value}.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs@_*) => MergeStrategy.discard
      case "module-info.class" => MergeStrategy.discard
      case x =>
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
    }
  )
  .dependsOn(core, datawrapper, ui)

lazy val core = project
  .settings(
    name := "CDMiA-core",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
  )

lazy val datawrapper = project
  .settings(
    name := "CDMiA-datawrapper",
    libraryDependencies += "org.postgresql" % "postgresql" % "42.7.3",
    libraryDependencies += "org.neo4j.driver" % "neo4j-java-driver" % "5.26.3",
    libraryDependencies += "io.zonky.test" % "embedded-postgres" % "2.0.7" % Test,
    libraryDependencies += "org.neo4j" % "neo4j" % "5.20.0" % Test,
    libraryDependencies += "org.neo4j.procedure" % "apoc-core" % "5.20.0" % Test,
    libraryDependencies += "org.neo4j.procedure" % "apoc-extended" % "5.20.0" % Test,
    libraryDependencies += "org.neo4j.procedure" % "apoc-processor" % "5.20.0" % Test,
    libraryDependencies += "org.neo4j.procedure" % "apoc-common" % "5.20.0" % Test,
    assemblyJarName in assembly := s"CDMiA-DataWrapper-${version.value}.jar",
  )
  .dependsOn(core)

lazy val ui = project
  .settings(
    name := "CDMiA-UI",
    libraryDependencies += "com.brunomnsilva" % "smartgraph" % "1.0.0",
    libraryDependencies += "org.openjfx" % "javafx-base" % "21.0.1",
    libraryDependencies += "org.openjfx" % "javafx-controls" % "21.0.1",
    libraryDependencies += "net.sf.jung" % "jung-api" % "2.1.1",
    libraryDependencies += "net.sf.jung" % "jung-visualization" % "2.1.1",
    libraryDependencies += "net.sf.jung" % "jung-algorithms" % "2.1.1",
    libraryDependencies += "net.sf.jung" % "jung-graph-impl" % "2.1.1",
    Compile / run / fork := true,
    assembly / mainClass := Some("cdmia.ui.Main"),
    assemblyJarName in assembly := s"CDMiA-UI-${version.value}.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case "module-info.class" => MergeStrategy.discard
      case x =>
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
    }
  )
  .dependsOn(core, datawrapper)

