import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

val cross = new utest.jsrunner.JsCrossBuild(
    organization := "com.lihaoyi",
    version := "0.1.0",
    name := "crdt",
    scalaVersion := "2.10.4",
    autoCompilerPlugins := true,
    addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided"
    ),
    publishArtifact in Test := false,
    publishTo := Some("releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"),
    pomExtra :=
      <url>https://github.com/lihaoyi/crdt</url>
      <licenses>
        <license>
          <name>MIT license</name>
          <url>http://www.opensource.org/licenses/mit-license.php</url>
        </license>
      </licenses>
      <scm>
        <url>git://github.com/lihaoyi/crdt.git</url>
        <connection>scm:git://github.com/lihaoyi/crdt.git</connection>
      </scm>
      <developers>
        <developer>
          <id>lihaoyi</id>
          <name>Li Haoyi</name>
          <url>https://github.com/lihaoyi</url>
        </developer>
      </developers>
)

lazy val root = cross.root

lazy val js = cross.js.settings(
  requiresDOM := false
)

lazy val jvm = cross.jvm

