val catsVersion = "0.6.1"
val catsAll = "org.typelevel" %% "cats" % catsVersion
val macroParadise = compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
val kindProjector = compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")
val resetAllAttrs = "org.scalamacros" %% "resetallattrs" % "1.0.0-M1"
val scalacheck = "org.scalacheck" %% "scalacheck" % "1.12.4"

lazy val root = (project in file("."))
  .settings(
    organization := "com.qf",
    name := "Cats Test",
    scalaVersion := "2.11.7",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      catsAll,
      scalacheck % Test,
      macroParadise, kindProjector, resetAllAttrs
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:_"
    )
  )
