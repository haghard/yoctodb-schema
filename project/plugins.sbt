addSbtPlugin("com.scalapenos"     %   "sbt-prompt"      % "1.0.2")
addSbtPlugin("org.scalameta"      %   "sbt-scalafmt"    % "2.4.2")
addSbtPlugin("de.heikoseeberger"  %   "sbt-header"      % "5.0.0")
addSbtPlugin("ch.epfl.scala"      % "sbt-scalafix"      % "0.9.26")
addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.3")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.2"
