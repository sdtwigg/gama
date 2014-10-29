scalaVersion in ThisBuild := "2.11.2"

scalacOptions in ThisBuild ++= Seq("-feature","-deprecation","-unchecked","-language:reflectiveCalls")

libraryDependencies in ThisBuild ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.11.2",
  "org.scalamacros" %% "resetallattrs" % "1.0.0-M1",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
)
