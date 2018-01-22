name := "torpedo"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

assemblyJarName in assembly := "torpedo.jar"

mainClass in assembly := Some("torpedo.main.Main")
