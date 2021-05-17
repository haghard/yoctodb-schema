import sbt._

lazy val `yoctodb-schema` = (project in file(".")).settings(commonSettings)

lazy val scalacSettings = Seq(
  scalacOptions ++= Seq(
    "-target:jvm-14",
    //"-deprecation",                    // Emit warning and location for usages of deprecated APIs.
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-encoding", "UTF-8",                // Specify character encoding used by source files.
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",              // Warn if a local definition is unused.
    "-Ywarn-unused:params",              // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates",            // Warn if a private member is unused.
    "-Ywarn-value-discard",              // Warn when non-Unit expression results are unused.
    "-Ymacro-annotations"
  )
)

lazy val commonSettings = scalacSettings ++ Seq(
  name := "yoctodb-schema",
  organization := "haghard",
  version := "0.0.1-SNAPSHOT",
  startYear := Some(2021),
  //sbt headerCreate
  licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  scalaVersion := "2.13.5",
  headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
  headerLicense  := Some(HeaderLicense.Custom(
    """|Copyright (c) 2021 by Vadim Bondarev
       |This software is licensed under the Apache License, Version 2.0.
       |You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.
       |""".stripMargin
  ))
)

libraryDependencies ++= Seq(
  "dev.zio" %%% "izumi-reflect" % "1.1.1",
  "com.typesafe" % "config" % "1.4.1",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.3",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.yandex.yoctodb" % "yoctodb-core" % "0.0.19",
  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",
  ("com.lihaoyi" % "ammonite" % "2.3.8-65-0f0d597f"  % "test").cross(CrossVersion.full)
)

resolvers ++= Seq(
  Resolver.jcenterRepo,
  "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"
)

promptTheme := ScalapenosTheme

scalafmtOnCompile := true

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main().run() }""")
  Seq(file)
}.taskValue

PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value
)


// Scalafix


ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0"

Global / semanticdbEnabled := true
Global / semanticdbVersion := scalafixSemanticdb.revision
Global / watchAntiEntropy := scala.concurrent.duration.FiniteDuration(10, java.util.concurrent.TimeUnit.SECONDS)


addCommandAlias("sfix", "scalafix OrganizeImports; test:scalafix OrganizeImports")