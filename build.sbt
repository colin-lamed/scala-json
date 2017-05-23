lazy val appVersion = "0.0.1-SNAPSHOT"

lazy val catsVersion = "0.9.0"
lazy val fastparseVersion = "0.4.3"

val shared = Seq(
  organization := "me.colinpassiv",
  name         := "scala-json",
  version      := appVersion,
  scalaVersion := "2.12.1",
  libraryDependencies ++= Seq(
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
  ),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  scalacOptions ++= Seq(
    //"-Xlog-implicits",
    "-Xlint",
    "-deprecation",
    "-feature",
    "-language:higherKinds",
    "-Xfuture",
    "-Ywarn-unused-import",
    "-Ywarn-value-discard"
  )
)

lazy val scalaJson = crossProject.in(file("."))
  .settings(shared:_*)
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.lihaoyi"   %% "fastparse"   % fastparseVersion,
      "org.typelevel" %% "cats-core"   % catsVersion
    )
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "com.lihaoyi"   %%% "fastparse"   % fastparseVersion,
      "org.typelevel" %%% "cats-core"   % catsVersion
    )
  )

lazy val scalaJsonJVM = scalaJson.jvm

lazy val scalaJsonJS = scalaJson.js

lazy val root = project.in(file("."))
  .settings(shared:_*)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi"   %% "fastparse"   % fastparseVersion,
      "org.typelevel" %% "cats-core"   % catsVersion,
      // Test
      "org.scalatest"  %% "scalatest"  % "3.0.0"     % Test,
      "org.scalacheck" %% "scalacheck" % "1.13.4"    % Test,
      "org.typelevel"  %% "discipline" % "0.7.2"     % Test,
      "org.typelevel"  %% "cats-laws"  % catsVersion % Test
    ),
    publishArtifact := false,
    publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
  )
  .aggregate(scalaJsonJVM, scalaJsonJS)
