import sbt._
import Keys._
import io.Source
import Classpaths.managedJars

// All credits for some parts of the code go to philcali
// @see https://github.com/philcali/sbt-lwjgl-plugin/
object ScageBuild extends Build {
    lazy val phys2DPatch = TaskKey[Unit]("phys2D-patch", "The phys2d dependency pom is broken. Patch aims to fix it")

    lazy val nativesExtract = TaskKey[Unit]("natives-extract", "Extracts LWJGL Native JAR file")

    def defineOs = System.getProperty("os.name").toLowerCase.take(3).toString match {
    	case "lin" => ("linux", "so")
    	case "mac" | "dar" => ("osx", "lib")
    	case "win" => ("windows", "dll")
    	case "sun" => ("solaris", "so")
    	case _ => ("unknown", "")
    }

    lazy val baseSettings: Seq[Setting[_]] = Defaults.defaultSettings ++ Seq(
	phys2DPatch <<= (streams, ivyPaths) map { (s, ivys) =>
        	val base = ivys.ivyHome.getOrElse(Path.userHome / ".ivy2")

       	        val path = base / "cache" / "phys2d" / "phys2d" / "ivy-060408.xml"

        	if (path.exists) {
                	s.log.info("Patching %s ..." format(path))
                	val pattern = "zip".r
                	val ivysource = Source.fromFile(path)
                	val text = ivysource.getLines.mkString
                	val writer = new java.io.FileWriter(path)
                	writer.write(pattern.replaceAllIn(text, "jar"))
                	writer.close
                	s.log.info("Done.")
        	} else {
                	s.log.warn("Update might fail. This is expected.")
                	s.log.warn("Please run update one more time.")
        	}
    	},

	nativesExtract <<= (streams, classpathTypes, update) map { (s, ct, up) =>
        	val natives = managedJars(Compile, ct, up) map { _.data } find { (j) =>
            j.getName.startsWith("lwjgl-platform") && j.getName.endsWith("%s.jar".format(defineOs._1))
          }
        	natives map { (e) =>
                	val target = file(".") / "libs" / "natives"
                	s.log.info("Extracting LWJGL natives to " + target)
                	IO.unzip(e, target)
        	} getOrElse {
                	s.log.warn("Unable to find LWJGL natives jar, try to update again.")
        	}
    	},

	// TODO should run on "update" only.
	update <<= update dependsOn phys2DPatch,
	// TODO : UH ?
	run <<= run in Runtime dependsOn nativesExtract,
	fork := true,
	javaOptions ++= Seq("-Djava.library.path=%s".format(file(".") / "libs" / "natives"), "-DLWJGL_DISABLE_XRANDR=true"),
	javaOptions in Test ++= Seq("-Dapp.properties=scagetest-properties.txt")
    )

    lazy val project = Project (
    	"scage",
    	file ("."),
   	settings = baseSettings
    )

}
