import sbt._

lazy val `yoctodb-schema` =
  (project in file(".")).settings(commonSettings)

lazy val scalac3Settings = Seq(
  scalacOptions ++= Seq(
    //"-target:jvm-14",
    "-feature",
    "-language:implicitConversions",
    "-unchecked",
    "-deprecation",
    //"-Xfatal-warnings",
    //"-Yexplicit-nulls",  //explicit-nulls is enabled
    //"-Ysafe-init",
    "-Ykind-projector",
    "-rewrite",
    "-indent",
    "-source",
    "future"
  )
)

lazy val commonSettings = scalac3Settings ++ Seq(
  name := "yoctodb-schema",
  organization := "haghard",
  version := "0.0.1-SNAPSHOT",
  startYear := Some(2021),

  Test / parallelExecution := false,
  run / fork := false,

  //sbt headerCreate
  licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  scalaVersion := "3.0.1",
  headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
  headerLicense  := Some(HeaderLicense.Custom(
    """|Copyright (c) 2021 by Vadim Bondarev
       |This software is licensed under the Apache License, Version 2.0.
       |You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.
       |""".stripMargin
  ))
)

resolvers ++= Seq(Resolver.jcenterRepo, "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/")

unmanagedBase := baseDirectory.value / "lib"

libraryDependencies ++= Seq(
  //"dev.zio" %% "izumi-reflect" % "1.1.2",
  "com.typesafe" % "config" % "1.4.1",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.yandex.yoctodb" % "yoctodb-core" % "0.0.19",
  "dev.zio" %% "zio-prelude"  % "1.0.0-RC5",

  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",

  //https://repo1.maven.org/maven2/com/lihaoyi/ammonite_3.0.1/2.4.0-11-5b9ff5e7/
  ("com.lihaoyi" % "ammonite"  % "2.4.0-14-4824b429"  % "test").cross(CrossVersion.full)
)

//Compile / scalacOptions --= Seq("-Xfatal-warnings", "-Ywarn-unused:imports", "-Yno-imports", "-deprecation")

promptTheme := ScalapenosTheme

scalafmtOnCompile := true

Test / sourceGenerators += Def.task {
  val file = (Test / sourceManaged).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main().run() }""")
  Seq(file)
}.taskValue

Compile / PB.targets := Seq(scalapb.gen() -> (Compile / sourceManaged).value)

addCommandAlias("c", "compile")
