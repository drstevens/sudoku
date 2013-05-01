import sbt._
import sbt.Keys._

object SudokuBuild extends Build {

  lazy val sudoku = Project(
    id = "sudoku",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "Sudoku",
      organization := "daverstevens",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.1",
      scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-language:postfixOps")
      // add other settings here
    )
  )
}
