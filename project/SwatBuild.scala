import sbt._
import Keys._

object WidelambBuild extends Build
{
	lazy val widelambProject =
        Project(
            "widelamb", 
			file("."), 
			settings = Defaults.defaultSettings ++ Seq(
				version := "0.1",
				scalaVersion := "2.10.0",
				scalacOptions ++= Seq(
					"-deprecation",
					"-unchecked",
					"-feature",
					"-encoding", "utf8",
					"-language:implicitConversions"
				),
				libraryDependencies ++= Seq(
					"org.scalatest" % "scalatest_2.10.0" % "1.8"
				)
			)
		)
}
