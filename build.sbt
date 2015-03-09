name := "fp-in-scala"

version := "0.0.1-SNAPSHOT"

organization := "net.janvsmachine"

scalaVersion := "2.11.6"

libraryDependencies ++= {
  Seq(
    "org.scalatest"       %% "scalatest"            % "2.2.2" % Test,
    "junit"               %  "junit"                % "4.11" % Test,
    "com.novocode"        %  "junit-interface"      % "0.11" % Test,
    "org.mockito"         %  "mockito-core"         % "1.9.5" % Test,
    "org.scalacheck"      %% "scalacheck"           % "1.11.6" % "test"
  )
}
