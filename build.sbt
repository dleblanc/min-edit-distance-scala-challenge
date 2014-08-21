name := """skel-scala"""

version := "1.0"

scalaVersion := "2.11.1"

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.2" % "test"

// Always pair up with Scalatest-dependent versions
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
