import sbt._
import Keys._

object BuildSettings {
  val paradiseVersion = "2.0.1"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
  )
}

object MacroBuild extends Build {
  import BuildSettings._

  lazy val test           = Project("test",           file("test_project"),   settings = buildSettings) dependsOn(chiselfrontend, librarychisel)
  lazy val chiselfrontend = Project("chiselfrontend", file("chiselfrontend"), settings = buildSettings)
  lazy val librarychisel  = Project("librarychisel",  file("librarychisel"),  settings = buildSettings) dependsOn(chiselfrontend)

}
