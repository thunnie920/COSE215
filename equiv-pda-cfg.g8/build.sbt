ThisBuild / scalaVersion := "3.1.1"
ThisBuild / scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-explain",
  "-explain-types",
  "-language:implicitConversions",
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "equiv-pda-cfg",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.3",
      "io.circe" %% "circe-generic" % "0.14.3",
      "io.circe" %% "circe-parser" % "0.14.3",
      "org.scalatest" %% "scalatest" % "3.2.15" % Test,
      ("org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0")
        .cross(CrossVersion.for3Use2_13),
    ),
    wartremoverClasspaths += "file://" + baseDirectory.value + "/../lib/warts.jar",
    wartremoverErrors ++= Seq(
      Wart.AsInstanceOf, Wart.IsInstanceOf, Wart.Null, Wart.Return, Wart.Throw,
      Wart.Var, Wart.While, Wart.MutableDataStructures,
      Wart.custom("kuplrg.warts.TryCatch"),
    ),
    wartremoverExcluded ++= Seq(
      baseDirectory.value / "src" / "main" / "scala" / "kuplrg" / "basics.scala",
      baseDirectory.value / "src" / "main" / "scala" / "kuplrg" / "error.scala",
      baseDirectory.value / "src" / "test" / "scala" / "kuplrg" / "SpecBase.scala",
    ),
  )

run := (root / Compile / run).evaluated
test := (root / Test / test).value
Test / testOptions += Tests
  .Argument("-fDG", baseDirectory.value + "/test-detail"),
