scalaVersion := "2.11.5"

scalacOptions ++= Seq("-deprecation","-feature","-unchecked")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
