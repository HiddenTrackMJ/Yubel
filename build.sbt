import sbt.Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._



name := "yubel"

val scalaV = "2.12.6"
//val scalaV = "2.11.8"

val projectName = "yubel"
val projectVersion = "2019.02.25"

val projectMainClass = "org.seekloud.yubel.Boot"

def commonSettings = Seq(
  version := projectVersion,
  scalaVersion := scalaV,
  scalacOptions ++= Seq(
    //"-deprecation",
    "-feature"
  )
)

// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.{crossProject, CrossType}


lazy val shared = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("shared"))
  .settings(name := "shared")
  .settings(commonSettings: _*)

lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

// Scala-Js frontend
lazy val frontend = (project in file("frontend"))
  .enablePlugins(ScalaJSPlugin)
  .settings(name := "frontend")
  .settings(commonSettings: _*)
  .settings(
    inConfig(Compile)(
      Seq(
        fullOptJS,
        fastOptJS,
        packageJSDependencies,
        packageMinifiedJSDependencies
      ).map(f => (crossTarget in f) ~= (_ / "sjsout"))
    ))
  .settings(skip in packageJSDependencies := false)
  .settings(
    scalaJSUseMainModuleInitializer := false,
    libraryDependencies ++=     Seq(
      "io.circe" %%% "circe-core" % Dependencies.circeVersion,
      "io.circe" %%% "circe-generic" % Dependencies.circeVersion,
      "io.circe" %%% "circe-parser" % Dependencies.circeVersion,
      "org.scala-js" %%% "scalajs-dom" % Dependencies.scalaJsDomV,
      "in.nvilla" %%% "monadic-html" % Dependencies.monadicHtmlV,
      "com.lihaoyi" %%% "scalatags" % Dependencies.scalaTagsV,
      "com.github.japgolly.scalacss" %%% "core" % Dependencies.scalaCssV,
      "org.seekloud" %%% "byteobject" % "0.1.1"
    )
  )
  .dependsOn(sharedJs)


lazy val frontendAdmin = (project in file("frontendAdmin"))
  .enablePlugins(ScalaJSPlugin)
  .settings(name := "frontendAdmin")
  .settings(commonSettings: _*)
  .settings(
    inConfig(Compile)(
      Seq(
        fullOptJS,
        fastOptJS,
        packageJSDependencies,
        packageMinifiedJSDependencies
      ).map(f => (crossTarget in f) ~= (_ / "sjsout"))
    ))
  .settings(skip in packageJSDependencies := false)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % Dependencies.circeVersion,
      "io.circe" %%% "circe-generic" % Dependencies.circeVersion,
      "io.circe" %%% "circe-parser" % Dependencies.circeVersion,
      "org.scala-js" %%% "scalajs-dom" % Dependencies.scalaJsDomV,
      "io.suzaku" %%% "diode" % "1.1.2",
      "com.lihaoyi" %%% "scalatags" % Dependencies.scalaTagsV withSources(),
      "com.github.japgolly.scalacss" %%% "core" % Dependencies.scalaCssV withSources(),
      "in.nvilla" %%% "monadic-html" % Dependencies.monadicHtmlV withSources(),
    )
  )
  .dependsOn(sharedJs)

// Akka Http based backend
lazy val backend = (project in file("backend")).enablePlugins(PackPlugin)
  .settings(commonSettings: _*)
  .settings(
    mainClass in reStart := Some(projectMainClass),
    javaOptions in reStart += "-Xmx2g"
  )
  .settings(name := "yubel")
  .settings(
    //pack
    // If you need to specify main classes manually, use packSettings and packMain
    //packSettings,
    // [Optional] Creating `hello` command that calls org.mydomain.Hello#main(Array[String])
    packMain := Map("yubel" -> projectMainClass),
    packJvmOpts := Map("yubel" -> Seq("-Xmx512m", "-Xms128m")),
    packExtraClasspath := Map("yubel" -> Seq("."))
  )
  .settings(
    libraryDependencies ++= Dependencies.backendDependencies  ++ Dependencies.grpcSeq
  )
  .settings {
    (resourceGenerators in Compile) += Def.task {
      val fastJsOut = (fastOptJS in Compile in frontend).value.data
      val fastJsSourceMap = fastJsOut.getParentFile / (fastJsOut.getName + ".map")
      Seq(
        fastJsOut,
        fastJsSourceMap
      )
    }.taskValue
  }
  .settings((resourceGenerators in Compile) += Def.task {
  Seq(
    (packageJSDependencies in Compile in frontend).value
    //(packageMinifiedJSDependencies in Compile in frontend).value
  )
}.taskValue)
  .settings(
    (resourceDirectories in Compile) += (crossTarget in frontend).value,
    watchSources ++= (watchSources in frontend).value
  )
  .settings {
    (resourceGenerators in Compile) += Def.task {
      val fastJsOut = (fastOptJS in Compile in frontendAdmin).value.data
      val fastJsSourceMap = fastJsOut.getParentFile / (fastJsOut.getName + ".map")
      Seq(
        fastJsOut,
        fastJsSourceMap
      )
    }.taskValue
  }
  .settings((resourceGenerators in Compile) += Def.task {
    Seq(
      (packageJSDependencies in Compile in frontendAdmin).value
      //(packageMinifiedJSDependencies in Compile in frontend).value
    )
  }.taskValue)
  .settings(
    (resourceDirectories in Compile) += (crossTarget in frontendAdmin).value,
    watchSources ++= (watchSources in frontendAdmin).value
  )
  .dependsOn(sharedJvm)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .aggregate(frontend, backend, frontendAdmin)
  .settings(name := "root")


