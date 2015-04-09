scalaVersion := "2.11.5"

scalacOptions ++= Seq("-deprecation","-feature","-unchecked")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test"
