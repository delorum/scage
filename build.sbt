name := "scage"

version := "0.9.3-SNAPSHOT"

scalaVersion := "2.10.1"

resolvers ++= Seq(
	"dunnololda's maven repo" at "https://raw.github.com/dunnololda/mvn-repo/master",
	"Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
	"FreeHEP Repository" at "http://java.freehep.org/maven2",
	"Scala-Tools Maven2 Repository" at "https://oss.sonatype.org/content/groups/scala-tools/"
)

libraryDependencies ++= Seq(
	"com.github.dunnololda" % "cli_2.10" % "1.5",
	"com.github.dunnololda" % "state_2.10" % "1.2",
  "com.github.dunnololda" % "mysimplelogger_2.10" % "1.2",
	"org.lwjgl.lwjgl" % "lwjgl" % "2.9.1",
	"org.lwjgl.lwjgl" % "lwjgl_util" % "2.9.1",
	"org.lwjgl.lwjgl" % "lwjgl-platform" % "2.9.1",
	"phys2d" % "phys2d" % "060408",
	"org.slick2d" % "slick2d-core" % "1.0.1",
	"org.slf4j" % "slf4j-api" % "1.6.1" % "compile",
	"com.novocode" % "junit-interface" % "0.10" % "test->default",
	"org.scala-lang" % "scala-swing" % "2.10.1"
)
