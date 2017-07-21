val cats = "org.typelevel" %% "cats" % "0.9.0"
val macroParadise = compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

lazy val root = (project in file("."))
  .settings(
    organization := "com.qf",
    name := "Cats Test",
    scalaVersion := "2.12.1",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies ++= Seq(cats, macroParadise),
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:_"
    )
  )
