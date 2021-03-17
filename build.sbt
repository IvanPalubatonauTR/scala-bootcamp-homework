name := "ScalaBootcampHomework"

version := "0.1"

scalaVersion := "2.13.4"

val catsVersion = "2.2.0"
val circeVersion = "0.13.0"
val scalaTestVersion = "3.1.0.0-RC2"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-optics" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion,
  "org.scalatestplus" %% "selenium-2-45" % scalaTestVersion,
  "com.codecommit" %% "cats-effect-testing-scalatest" % "0.4.1",
  "org.scalaj" %% "scalaj-http" % "2.4.2"
)

