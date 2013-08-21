name := "scage"

version := "0.9.3-SNAPSHOT"

scalaVersion := "2.10.1"

resolvers ++= Seq(
	"dunnololda's maven repo" at "https://raw.github.com/dunnololda/mvn-repo/master",
	"lwjgl" at "http://adterrasperaspera.com/lwjgl",
	"Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
	"FreeHEP Repository" at "http://java.freehep.org/maven2",
	"B2S Repository" at "http://b2s-repo.googlecode.com/svn/trunk/mvn-repo",
	"Scala-Tools Maven2 Repository" at "https://oss.sonatype.org/content/groups/scala-tools/"
)

libraryDependencies ++= Seq(
	"com.github.dunnololda" % "cli_2.10.1" % "1.2",
	"org.lwjgl" % "lwjgl" % "2.8.2",
	"org.lwjgl" % "lwjgl-util" % "2.8.2",
	"org.lwjgl" % "lwjgl-jinput" % "2.8.2",
	"slick" % "slick" % "274" exclude("org.lwjgl", "lwjgl-native"),
	"org.slf4j" % "slf4j-api" % "1.6.1" % "compile",
	"com.novocode" % "junit-interface" % "0.10" % "test->default",
	"org.scala-lang" % "scala-swing" % "2.10.1"
)
