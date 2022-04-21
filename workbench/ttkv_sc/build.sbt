lazy val root = (project in file("."))
  .settings(
    name := "ttkv",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "1.3.0",
      "org.typelevel" %% "alleycats-core" % "1.6.0",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
    ),
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:postfixOps",
      "-language:higherKinds",
      "-Ypartial-unification"
    )
  )
