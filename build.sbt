name := "squeryl"

description := "A Scala ORM and DSL for talking with Databases using minimum verbosity and maximum type safety"

organization := "com.github.novamage"

javacOptions := Seq("-source", "1.8", "-target", "1.8")

//only release *if* -Drelease=true is passed to JVM
version := {
  val v = version.value
  val release = System.getProperty("release") == "true"
  if (release)
    v
  else {
    val suffix = Option(System.getProperty("suffix"))
    val i = (v.indexOf('-'), v.length) match {
      case (x, l) if x < 0 => l
      case (x, l) if v substring (x + 1) matches """\d+""" => l //patch level, not RCx
      case (x, _) => x
    }
    v.substring(0, i) + "-" + (suffix getOrElse "SNAPSHOT")
  }
}

parallelExecution := false

publishMavenStyle := true

scalaVersion := "2.13.12"

scalacOptions in(Compile, doc) ++= {
  val base = (baseDirectory in LocalRootProject).value.getAbsolutePath
  val hash = sys.process.Process("git rev-parse HEAD").lineStream_!.head
  Seq("-sourcepath", base, "-doc-source-url", "https://github.com/squeryl/squeryl/tree/" + hash + "€{FILE_PATH}.scala")
}

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:reflectiveCalls",
  "-language:existentials"
)


licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

homepage := Some(url("http://squeryl.org"))

pomExtra := <scm>
  <url>git@github.com:squeryl/squeryl.git</url>
  <connection>scm:git:git@github.com:squeryl/squeryl.git</connection>
</scm>
  <developers>
    <developer>
      <id>NovaMage</id>
      <name>Angel Blanco</name>
      <url>https://github.com/novamage</url>
    </developer>
    <developer>
      <id>max-l</id>
      <name>Maxime Lévesque</name>
      <url>https://github.com/max-l</url>
    </developer>
    <developer>
      <id>davewhittaker</id>
      <name>Dave Whittaker</name>
      <url>https://github.com/davewhittaker</url>
    </developer>
  </developers>

releasePublishArtifactsAction := PgpKeys.publishSigned.value

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

libraryDependencies ++= Seq(
  "cglib" % "cglib-nodep" % "3.3.0",
  "com.h2database" % "h2" % "1.4.197" % "provided",
  "mysql" % "mysql-connector-java" % "5.1.47" % "provided",
  "org.postgresql" % "postgresql" % "42.1.4.jre7" % "provided",
  "net.sourceforge.jtds" % "jtds" % "1.3.1" % "provided",
  "org.apache.derby" % "derby" % "10.11.1.1" % "provided",
  "org.xerial" % "sqlite-jdbc" % "3.25.2" % "test",
  "org.json4s" %% "json4s-scalap" % "3.6.12",
  "org.scala-lang.modules" %% "scala-xml" % "1.3.1"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest-funsuite" % "3.2.17" % "test",
  "org.scalatest" %% "scalatest-shouldmatchers" % "3.2.17" % "test",
)
