lazy val root = project.in(file("."))

scalaVersion := "2.11.8"
name := "mediorganize"
normalizedName := "mediorganize"
version := "0.1-SNAPSHOT"
organization := "com.yakticus"
crossScalaVersions := Seq("2.10.5", "2.11.7")
scalacOptions ++= Seq("-deprecation", "-feature")
homepage := Some(url("https://github.com/yakticus/mediorganize"))
licenses += ("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
scmInfo := Some(ScmInfo(
  url("https://github.com/yakticus/mediorganize"),
  "scm:git:git@github.com:yakticus/mediorganize.git",
  Some("scm:git:git@github.com:yakticus/mediorganize.git")))
publishMavenStyle := true
publishArtifact in Test := false
pomIncludeRepository := { _ => false }
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomExtra := (
    <developers>
      <developer>
        <id>yakticus</id>
        <name>Julie Pitt</name>
        <url>https://github.com/yakticus</url>
      </developer>
    </developers>
  )

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "ammonite-ops" % "0.8.1",
  "com.lihaoyi" %% "upickle" % "0.4.3"
)
