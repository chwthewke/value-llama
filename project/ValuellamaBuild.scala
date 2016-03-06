import sbt._
import sbt.Keys._
import com.typesafe.sbt.SbtScalariform._
import com.typesafe.sbteclipse.core.EclipsePlugin._
import scalariform.formatter.preferences._
import scoverage.ScoverageSbtPlugin
import sbtbuildinfo.Plugin._

object ValuellamaBuild extends Build {

  object Dependencies {
    val scalazVersion = "7.2.0"

    val scalaz = "org.scalaz" %% "scalaz-core" % scalazVersion withSources () withJavadoc ()

    val scalatest = "org.scalatest" %% "scalatest" % "3.0.0-M15" % "test" withSources () withJavadoc ()

    val scalacheck = Seq(
      "org.scalacheck" %% "scalacheck" % "1.13.0" % "test" withSources () withJavadoc (),
      "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test" withSources () withJavadoc ()
    )

    val monocleVersion = "1.2.0"

    val monocle = Seq(
      "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-generic" % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-state" % monocleVersion
    )

  }

  override def settings = super.settings ++ Seq(
    addCompilerPlugin( "org.spire-math" %% "kind-projector" % "0.7.1" cross CrossVersion.binary ) )

  lazy val valuellamaScalariformSettings = scalariformSettings ++ Seq(
    ScalariformKeys.preferences := defaultPreferences
      .setPreference( AlignSingleLineCaseStatements, true )
      .setPreference( SpaceBeforeColon, true )
      .setPreference( SpaceInsideParentheses, true )
  )

  lazy val sharedSettings =
    Seq(
      organization := "net.chwthewke",
      scalaVersion := "2.11.7" )

  lazy val valuellamaSettings =
    Defaults.coreDefaultSettings ++
      SbtBuildInfo.buildSettings( "net.chwthewke.valuellama" ) ++
      SbtEclipse.buildSettings ++
      valuellamaScalariformSettings ++
      sharedSettings ++
      Seq(
        libraryDependencies ++= Seq(
          Dependencies.scalatest,
          Dependencies.scalaz ) ++
          Dependencies.monocle ++
          Dependencies.scalacheck,
        scalacOptions ++= Seq( "-feature", "-deprecation" ),
        unmanagedSourceDirectories in Compile := ( scalaSource in Compile ).value :: Nil,
        unmanagedSourceDirectories in Test := ( scalaSource in Test ).value :: Nil
      )

  lazy val valueLlamaAll = Project(
    id = "value-llama-all",
    base = file( "." ),
    settings = Defaults.coreDefaultSettings ++
      sharedSettings ++
      Seq( name := "value-llama-all" )
  ).aggregate(
      valuellama
    )

  lazy val valuellama = Project(
    id = "value-llama",
    base = file( "value-llama" ),
    settings = valuellamaSettings ++
      Seq(
        name := "value-llama",
        mainClass := Some( "net.chwthewke.valuellama.Main" ),
        initialCommands := """|import net.chwthewke.valuellama._
                              |import scalaz._,Scalaz._""".stripMargin,
        buildInfoObject := "ValuellamaBuildInfo"
      )
  )
}
