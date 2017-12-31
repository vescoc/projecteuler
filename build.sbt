name := "ProjectEuler"

scalaVersion := "2.12.4"
scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

fork in run := true

javaOptions in run += "-Xmx8G"

excludeFilter in unmanagedSources := HiddenFileFilter || ".#*" || "*~"
