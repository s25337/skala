name := "pekko2"
version := "1.0.0"

scalaVersion := "3.7.4"

Compile / scalacOptions ++= Seq(
   "-deprecation",         // Emit warning and location for usages of deprecated APIs.
   "-explain",             // Explain errors in more detail.
   "-feature",             // Emit warning and location for usages of features that should be imported explicitly.
   "-print-lines",         // Show source code line numbers.
   "-unchecked",           // Enable additional warnings where generated code depends on assumptions
   "-Xfatal-warnings",     // Fail the compilation if there are any warnings.
   "-source:3.7",
   "-encoding", "utf8",
)

/*
  Currently available compiler options and their meanings can be looked-up in:

  https://github.com/lampepfl/dotty/blob/main/compiler/src/dotty/tools/dotc/config/ScalaSettings.scala
*/

libraryDependencies ++= {
  val pekkoV = "1.3.0"
  Seq(
    "org.apache.pekko" %% "pekko-actor" % pekkoV,
    "org.apache.pekko" %% "pekko-slf4j" % pekkoV,
    //"ch.qos.logback" % "logback-classic" % "1.3.8", // for JDK ver. < 11
    "ch.qos.logback" % "logback-classic" % "1.5.21" // for Java ver. >= 11
  )
}

// Option needed for JDK >= 24
//reStart / javaOptions += "--sun-misc-unsafe-memory-access=allow"

