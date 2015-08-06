name := "lwm-brindex"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.5"

libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "1.1.5"

libraryDependencies ++= Seq("com.propensive" %% "rapture-json-play" % "1.1.0")
