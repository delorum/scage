![Scage Logo](http://dl.dropbox.com/u/11297078/public_pics/scage-logo.png)

Introduction
------------

Scage is a framework to write simple 2D opengl games. It is written in [Scala](http://scala-lang.org/) and based on several java libraries:

 - [phys2d](http://phys2d.cokeandcode.com/) as a physics engine
 - [lwjgl](http://lwjgl.org) as an opengl wrapper
 - [slick](http://slick.cokeandcode.com/) as a resource and texture loader

The main purpose of this project is to give a convenient tool for game-developers to write a code of pure functionality without any boilerplate.

Features
--------

 - Architechture similar to actors framework with different kinds of tasks executing on different stages of app lifecycle. Simililar to actors these tasks are anonymous functions, and you can add and remove them in runtime in any scope of your app. Its all singlethreaded, so you dont have to mess with messages.
 - Vast drawing library for any kinds of 2D opengl primitives.
 - Loading and utilizing fonts from ttf-files (based on 'Slick2D' api but with improvements).
 - i18n: loading strings and even the whole interfaces from xml files. Runtime language change.
 - Framework to build in-game interfaces from xml files of simple structure.
 - App settings can be specified in a text files as a key-value pairs. Lots of engine options are set that way (alongside with the standard possibility to set them as parameters) allowing fine-tuning without app rebuilding.
 - Tracers framework: easy game objects tracking and interacting on a two-dimensional game map.
 - Lightweight wrapper upon phys2d engine.
 - Easy app building/deploing (as a standalone or via webstart) using maven infrastructure.
 - Multiple platforms support: Windows, Linux, Mac, Solaris (thanks to Java and lwjgl actually). Similar build process for any platform (with maven).
 - Client/server network api upon actors with simple text protocol based on json format.
 
Please read the project wiki and especially see [Examples](https://github.com/dunnololda/scage/wiki/Examples) page to learn more!

Hello World Example
-------------------

###Rotating 'Hello World!' label

    import net.scage.ScageScreenApp
    import net.scage.ScageLib._
    import net.scage.support.Vec

    object HelloWorldExample extends ScageScreenApp("Hello World") {
      private var ang = 0f
      actionStaticPeriod(100) {
        ang += 5
      }

      backgroundColor = BLACK
      render {
        openglMove(windowSize/2)
        openglRotate(ang)
        print("Hello World!", Vec(-50, -5), GREEN)
      }
    }

!['rotating "Hello World!" demo'](http://dl.dropbox.com/u/11297078/public_pics/rotating_hello.png)

###Network api example

Scage's network api represents an asynchronous (in general) client-server model (using Scala's actors framework) and is based on 
simple text protocol over TCP/IP. Clients and server are send messages to each other in JSON format. 

In the example below client sends to server random 2d vectors and server sends back corresponded normalized values.

    import net.scage.ScageApp
    import net.scage.support.net.{NetClient, NetServer}
    import net.scage.support.{Vec, State}

    object EchoExample extends ScageApp("Echo") {
      NetServer.startServer(
        port = 9800,
        onNewConnection = {
          client => client.send(State("hello" -> "send me vec and I send you back its n!"))
          (true, "")
        },
        onClientDataReceived = {
          (client, received_data) => received_data.neededKeys {
            case ("vec", vec:Vec) => client.send(State(("n" -> vec.n)))
          }
        }
      )

      NetClient.startClient(
        server_url = "localhost",
        port = 9800,
        onServerDataReceived = {
          received_data => received_data.neededKeys {
            case ("hello", hello_msg) =>
              val random_vec = Vec((math.random*100).toInt, (math.random*100).toInt)
              println("sending vec: "+random_vec)
              NetClient.send(State(("vec" -> random_vec)))
            case ("n", n:Vec) =>
              println("received n: "+n)
              println("waiting 5 sec...")
              Thread.sleep(5000)
              val random_vec = Vec((math.random*100).toInt, (math.random*100).toInt)
              println("sending vec: "+random_vec)
              NetClient.send(State("vec" -> random_vec))
          }
        }
      )

      dispose {
        NetServer.stopServer()
        NetClient.stopClient()
      }
    }
    
More examples
-------------

See [Examples Page](https://github.com/dunnololda/scage/wiki/Examples)

Usage
------------

###For Maven users

Add to your pom.xml the following:

      <repositories>
      ...
          <repository>
            <id>scage</id>
            <name>Scage Maven Repo</name>
            <url>http://scage.googlecode.com/svn/maven-repository</url>
          </repository>
      </repositories>
      ...
      <dependencies>
      ...
          <dependency>
              <groupId>su.msk.dunno</groupId>
              <artifactId>scage</artifactId>
              <version>0.9</version>
              <scope>compile</scope>
          </dependency>
      </dependencies>

You can use archetype to create new scage project stub:

    $ mvn archetype:generate -DgroupId=my.company -DartifactId=app -Dversion=0.1 -Dpackage=my.company.app -DarchetypeGroupId=scage -DarchetypeArtifactId=project-archetype -DarchetypeVersion=0.9 -DarchetypeRepository=http://scage.googlecode.com/svn/maven-repository
    
To launch app from the project stub you can type:

    $ mvn clean test
    
This project stub has two profiles in its pom.xml for app building. To build a standalone app type in your console:

    $ mvn clean package -Pbuild
    
Or just:

    $ mvn clean package

as "build" is a default profile.

To build a webstart app type:

    $ mvn clean package -Pwebstart 
   
This command will create "jnlp" folder in "target". Then you can upload this folder to your host.

You also can use some IDE with good Maven and Scala support (for example, [IntelliJ IDEA](http://www.jetbrains.com/idea/)).

###For non-Maven users.

There is a beta SBT integration which is basicly working, but needs some more testing, actually some issues may occur due to LWJGL version (LWJGL is quite complicated to integrate with SBT because of dependency management). Anyway, you can use it. Give me some feedback =).

First of all, you have to get the last version of Scage and publish it into your local Ivy Repository :

```
git clone https://github.com/dunnololda/scage.git
cd scage
sbt
update
update (yeah, one more time)
publish-local
```

Now you have Scage installed, congratulations ! You can create a minimal project. Here is the build.sbt you will need :

```
name := "foobar"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.10.1"

checksums in update := Nil

resolvers ++= Seq(
	"dunnololda's maven repo" at "https://raw.github.com/dunnololda/mvn-repo/master"
)

libraryDependencies ++= Seq(
	"scage" %% "scage" % "0.9.3-SNAPSHOT"
)

seq(slickSettings: _*)

slick.version := "274"
```

You have to use SBT LWJGL Plugin to get your project to compile, so put this into your project/plugins.sbt :

```
import sbt._

import Defaults._

libraryDependencies += sbtPluginExtra(
    m = "com.github.philcali" % "sbt-lwjgl-plugin" % "3.1.4",
    sbtV = "0.12",
    scalaV = "2.9.2"
)

```

Here we are, your project is set up. You can create your first Scage source into /src/main/scala/MyFirstSource.scala. Don't forget that due to SBT LWJGL Plugin, you have to run 'update' twice to compile your project.

Feedback
--------

Feel free to ask any questions by email or using issue tracker.

