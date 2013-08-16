name := "scage"

version := "0.9.3-SNAPSHOT"

scalaVersion := "2.10.1"

// We have to do that to pull LWJGL 2.1 required by Phys2D on old repository.
checksums in update := Nil

resolvers ++= Seq(
	"dunnololda's maven repo" at "https://raw.github.com/dunnololda/mvn-repo/master",
	"Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
	"Scala-Tools Maven2 Repository" at "https://oss.sonatype.org/content/groups/scala-tools/"
)

libraryDependencies ++= Seq(
	"com.github.dunnololda" % "cli_2.10.1" % "1.2",
	"org.slf4j" % "slf4j-api" % "1.6.1" % "compile",
	"com.novocode" % "junit-interface" % "0.10" % "test->default",
	"org.scala-lang" % "scala-swing" % "2.10.1"
)

seq(slickSettings: _*)

slick.version := "274"
