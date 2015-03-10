![Scage Logo](http://dl.dropbox.com/u/11297078/public_pics/scage-logo.png)

Introduction
------------

Scage is a framework to write simple 2D opengl games. It is written in [Scala](http://scala-lang.org/) and based on several java libraries:

 - [phys2d](https://code.google.com/p/phys2d/) as a physics engine
 - [lwjgl](http://lwjgl.org) as an opengl wrapper
 - [slick](https://code.google.com/p/phys2d/) as a resource and texture loader
 - 

The main purpose of this project is to give a convenient tool for game-developers to write a code of pure functionality without any boilerplate.

Features
--------

 - Architechture similar to actors framework with different kinds of tasks executing on different stages of app lifecycle. Simililar to actors these tasks are anonymous functions, and you can add and remove them in runtime in any scope of your app. Its all singlethreaded, so you dont have to mess with messages.
 - Vast drawing library for any kinds of 2D opengl primitives.
 - Loading and utilizing fonts from ttf-files (based on 'Slick2D' api but with improvements).
 - i18n: loading strings and even the whole interfaces from xml files. Runtime language change.
 - Framework to build in-game interfaces from xml files of simple structure.
 - App settings can be specified in a text files as a key-value pairs (moved to the external project: [scala-cli](https://github.com/dunnololda/scala-cli)). Lots of engine options are set that way (alongside with the standard possibility to set them as parameters) allowing fine-tuning without app rebuilding.
 - Tracers framework: easy game objects tracking and interacting on a two-dimensional game map.
 - Lightweight wrapper upon phys2d engine.
 - Easy app building/deploing (as a standalone or via webstart) using maven infrastructure.
 - Multiple platforms support: Windows, Linux, Mac, Solaris (thanks to Java and lwjgl actually). Similar build process for any platform (with maven).
 - Client/server network api upon actors with simple text protocol based on json format (moved to the external project: [simple-net](https://github.com/dunnololda/simple-net)).
 
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

More examples
-------------

See [Examples Page](https://github.com/dunnololda/scage/wiki/Examples)

Usage
------------

###For Maven users

You can use the scage archetype to create a new scage project stub:

    $ mvn archetype:generate -DarchetypeGroupId=scage -DarchetypeArtifactId=project-archetype -DarchetypeVersion=10.2 -DarchetypeRepository=https://raw.github.com/dunnololda/mvn-repo/master
    
Answer questions about groupId, artifactId, version and default package and a new folder named as {artifactId} will be created. Inside will be ready to run and deploy small application - simple light cycles game based on the Tron movie.
    
To launch app from the project stub you can type:

    $ mvn clean test
    
This project stub has two profiles in its pom.xml for app building. To build a standalone app type in your console:

    $ mvn clean package -Pbuild -Dmaven.test.skip
    
Or just:

    $ mvn clean package -Dmaven.test.skip

as "build" is a default profile.

To build a webstart app type:

    $ mvn clean package -Pwebstart -Dmaven.test.skip 
   
This command will create "jnlp" folder in "target". Then you can upload this folder to your host.

###Intellij IDEA

You also can use some IDE with good Maven and Scala support (for example, [IntelliJ IDEA](http://www.jetbrains.com/idea/)). In Idea choose "Import Project", point to the created folder from the previous step and then choose: "Import project from external model" - "Maven". Then type "Next" a few times. Idea project will be created.

###For non-Maven users.

You can both :
 - Compile Scage with SBT
 - Use SBT in your own Scage projects with [SBT Scage Plugin](https://github.com/mvallerie/sbt-scage-plugin). Just follow the README :).

Feedback
--------

Feel free to ask any questions by email or using issue tracker.

