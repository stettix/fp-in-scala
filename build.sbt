name := "fp-in-scala"

version := "0.0.1-SNAPSHOT"

organization := "net.janvsmachine"

scalaVersion := "2.13.5"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:higherKinds",
)

libraryDependencies ++= {
  Seq(
    "org.scalatest"       %% "scalatest"            % "3.0.9" % Test,
    "org.scalacheck"      %% "scalacheck"           % "1.14.1" % Test
  )
}
