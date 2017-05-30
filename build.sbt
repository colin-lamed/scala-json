import sbtcrossproject.{crossProject, CrossType}

lazy val catsVersion = "0.9.0"
lazy val fastparseVersion = "0.4.3"
lazy val monocleVersion = "1.4.0"

val shared = Seq(
  organization := "me.colinpassiv",
  name         := "scala-json",
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

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("core"))
  .settings(shared:_*)
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.lihaoyi"   %% "fastparse"   % fastparseVersion,
      "org.typelevel" %% "cats-core"   % catsVersion,
      "com.github.julien-truffaut" %%  "monocle-core"  % monocleVersion,
      "com.github.julien-truffaut" %%  "monocle-macro" % monocleVersion,
      // Test
      "org.scalatest"  %% "scalatest"  % "3.0.1"     % Test,
      "org.scalacheck" %% "scalacheck" % "1.13.4"    % Test,
      "org.typelevel"  %% "discipline" % "0.7.2"     % Test,
      "org.typelevel"  %% "cats-laws"  % catsVersion % Test,
      "com.github.julien-truffaut" %%  "monocle-law"  % monocleVersion % Test
    )
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "com.lihaoyi"   %%% "fastparse"   % fastparseVersion,
      "org.typelevel" %%% "cats-core"   % catsVersion,
      "com.github.julien-truffaut" %%%  "monocle-core"  % monocleVersion,
      "com.github.julien-truffaut" %%%  "monocle-macro" % monocleVersion
    )
  )

lazy val coreJVM = core.jvm
lazy val coreJS  = core.js

lazy val root = project.in(file("."))
  .settings(shared:_*)
  .settings(
    publishArtifact := false,
    publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
  )
  .aggregate(coreJVM, coreJS)
