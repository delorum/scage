![Scage Logo](http://dl.dropbox.com/u/11297078/public_pics/scage-logo.png)

Latest Stable Version
---------------------

11.3

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

    import com.github.dunnololda.scage.ScageLib._

    object HelloWorldExample extends ScageScreenApp("Scage App", 640, 480) {
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

Engine brief description
------------------------

See [Engine methods overview](https://github.com/dunnololda/scage/wiki/Scage-methods-overview)

Usage
------------

###For Maven users

You can use the scage archetype to create a new scage project stub:

    $ mvn archetype:generate -DarchetypeGroupId=scage -DarchetypeArtifactId=project-archetype -DarchetypeVersion=11.3 -DarchetypeRepository=https://raw.github.com/dunnololda/mvn-repo/master

This utilize the maven's "archetype" feature - create a simple example project with all needed stuff. Answer some questions about its name and version and you are done.

For example:
groupId: mygroup
artifactId: myartifact
version: 0.1
package mygroup.myartifact

(these all names are not really important, you can choose anything)

In the end type Y, hit 'enter' and the folder "myartifact" will be created. Inside will be a small application, ready to compile, run and deploy - simple Light Cycles game based on the Tron movie.
    
To launch app from the project stub you can type:

    $ mvn clean test
    
This project stub has two profiles in its pom.xml for the app building. To build a standalone app type in your console:

    $ mvn clean package -Pbuild -Dmaven.test.skip
    
Or just:

    $ mvn clean package -Dmaven.test.skip

as "build" is a default profile.

To build a webstart app type:

    $ mvn clean package -Pwebstart -Dmaven.test.skip 
   
This command will create "jnlp" folder in "target". Then you can upload this folder to your host.

More info you can find in the readme file inside the project's root.

###OpenJDK

If you use OpenJDK (not Oracle JDK) you need to add the openjdk profile to all mvn commands above:

    $ mvn clean test -Popenjdk
    $ mvn clean package -Pbuild,openjdk
    $ mvn clean package -Pwebstart,openjdk

Additionally you need to install the package "icedtea-web" (this is the name in Archlinux, in Ubuntu it should be something similar).

###Intellij IDEA

You also can use some IDE with good Maven and Scala support (for example, [IntelliJ IDEA](http://www.jetbrains.com/idea/)). Here are the steps for IDEA:

Download Intellij IDEA Community Edition from there: https://www.jetbrains.com/idea/download/ (it's free).

Unzip it and run. Setup scala plugin.

Then in the main menu click "Import Project" - choose the folder "myartifact".
In the new window choose "Import project from external model" - "Maven". Then just hit "Next" several times.

Then wait for a while and IDEA will setup the project for you.

How to run it:

In the left panel go to src/main/scala/mygroup.myartifact. There is only one file - LightCyclesOffline.scala. Open it. Place the cursor in the row second to "object LightCyclesOffline extends ScageScreenApp("Light Cycles", 640, 480) {". 

Right click - Create LightCyclesOffline. In the new window in the "VM Options" type:

    -Djava.library.path=target/natives -DLWJGL_DISABLE_XRANDR=true -Dfile.encoding=UTF-8

Then click OK. Then for example right click again and choose Run LightCyclesOffline. IDEA will build and run the app.

###For non-Maven users.

You can both :
 - Compile Scage with SBT
 - Use SBT in your own Scage projects with [SBT Scage Plugin](https://github.com/mvallerie/sbt-scage-plugin). Just follow the README :).

Feedback
--------

Feel free to ask any questions by email or using issue tracker.

