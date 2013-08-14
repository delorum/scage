// @see http://stackoverflow.com/questions/11768730/how-to-inform-sbt-to-consume-specific-scala-version-for-plugins

import sbt._

import Defaults._

libraryDependencies += sbtPluginExtra(
    m = "com.github.philcali" % "sbt-lwjgl-plugin" % "3.1.4",
    sbtV = "0.12",
    scalaV = "2.9.2"
)

