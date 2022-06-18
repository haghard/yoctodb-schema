import sbt._

lazy val `yoctodb-schema` = (project in file(".")).settings(commonSettings)

lazy val scalac3Settings = Seq(
  //https://docs.scala-lang.org/scala3/guides/migration/options-new.html#standard-settings
  scalacOptions ++= Seq(
    //"-deprecation",
    "-feature",
    "-language:implicitConversions",
    "-unchecked",
    //"-Xfatal-warnings",
    //"-Yexplicit-nulls",
    //"-Wunused",
    "-Ywarn-unused",  //sfix
    "-Ykind-projector",
    "-Ysafe-init", //guards against forward access reference
  ) ++ Seq("-rewrite", "-indent") ++ Seq("-source", "future")
)

lazy val commonSettings = scalac3Settings ++ Seq(
  name := "yoctodb-schema",
  organization := "haghard",
  version := "0.0.1-SNAPSHOT",
  startYear := Some(2021),

  Test / parallelExecution := false,
  run / fork := false,

  Compile / console / scalacOptions --= Seq("-Wunused:_", "-Xfatal-warnings"),

  //https://github.com/OOPMan/collectioneer/blob/master/build.sbt
  //https://github.com/com-lihaoyi/Ammonite/issues/1241
  /*Test / sourceGenerators += Def.task {
    val file = (Test / sourceManaged).value / "amm.scala"
    IO.write(file,
      """object amm {
        |   def main(args: Array[String]) = ammonite.Main.main(args)
        |}""".stripMargin
    )
    Seq(file)
  }.taskValue,*/

  //sbt headerCreate
  licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  scalaVersion := "3.1.2",
  headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
  headerLicense  := Some(HeaderLicense.Custom(
    """|Copyright (c) 2021-22 by Vadim Bondarev
       |This software is licensed under the Apache License, Version 2.0.
       |You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.
       |""".stripMargin
  ))
)

resolvers ++= Seq(Resolver.jcenterRepo, "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/")

unmanagedBase := baseDirectory.value / "lib"

libraryDependencies ++= Seq(
  "com.typesafe"       %  "config"          % "1.4.2",
  "ch.qos.logback"     %  "logback-classic" % "1.2.11",
  "com.yandex.yoctodb" %  "yoctodb-core"    % "0.0.20",

  "dev.zio"   %% "izumi-reflect" % "2.1.0",

  "com.softwaremill.magnolia1_3" %% "magnolia" % "1.1.4",

  //Boilerplate Free Validations Using Scala 3 - Tamer Abdulradi: https://youtu.be/y2j-oZ8uHuo
  //https://github.com/tabdulradi/mazboot
  "com.abdulradi" %% "mazboot-types" % "0.5.0",


  //https://medium.com/scalac/inline-your-boilerplate-harnessing-scala-3-metaprogramming-without-macros-c106ef8d6dfb
  //https://github.com/arainko/ducktape
  "io.github.arainko" %% "ducktape" % "0.0.13",

  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf",

  //https://repo1.maven.org/maven2/com/lihaoyi/ammonite_3.0.1/2.4.0-11-5b9ff5e7/
  //https://repo1.maven.org/maven2/com/lihaoyi/ammonite_3.1.2/2.5.4-2-71d100df/
  //("com.lihaoyi" % "ammonite" % "2.5.4" % "test").cross(CrossVersion.full)
  //("com.lihaoyi" % "ammonite" % "2.5.4-2-71d100df" % "test").cross(CrossVersion.full)
)

//Compile / scalacOptions --= Seq("-Xfatal-warnings", "-Ywarn-unused:imports", "-Yno-imports", "-deprecation")

promptTheme := ScalapenosTheme

scalafmtOnCompile := true

/*
Test / sourceGenerators += Def.task {
  val file = (Test / sourceManaged).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main().run() }""")
  Seq(file)
}.taskValue
*/

Compile / PB.targets := Seq(scalapb.gen() -> (Compile / sourceManaged).value)

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0"

Global / semanticdbEnabled := true
Global / semanticdbVersion := scalafixSemanticdb.revision
Global / watchAntiEntropy := scala.concurrent.duration.FiniteDuration(10000, java.util.concurrent.TimeUnit.MILLISECONDS)

addCommandAlias("sfix", "scalafix OrganizeImports; test:scalafix OrganizeImports")
addCommandAlias("sFixCheck", "scalafix --check OrganizeImports; test:scalafix --check OrganizeImports")

addCommandAlias("c", "compile")
addCommandAlias("r", "reload")