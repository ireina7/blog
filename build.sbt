ThisBuild / organization := "blog"
ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := "2.13.6"

import Dependencies._
import Library._
import Operations._


lazy val blog = (project in file("."))
  .aggregate(server, client, shared)
  // .dependsOn(server, client)
  .settings(
    name := "blog",
    scalaVersion := "2.13.6",

    libraryDependencies += junit % Test,

    Compile/compile := (Compile/compile).dependsOn(compileOthers).value,
    // Compile/runner := (Compile/runner).dependsOn(server/Compile/runner).value
  )


lazy val server = (project in file("server"))
  .dependsOn(shared)
  .settings(
    name := "server",
    scalaVersion := "2.13.6",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-language:higherKinds",
      "-language:postfixOps",
      "-feature",
      "-Xfatal-warnings",
    ),

  libraryDependencies ++= Seq(

    circe,
    logBack,
    scalatags,

    // Http4s server
    Http4s.server,
    Http4s.client,
    Http4s.circe,
    Http4s.dsl,
    Http4s.scalatags,

    
    // Doobie functional JDBC layer
    Doobie.core,
    Doobie.postgres,
    Doobie.specs2,

    // For test
    Munit.munit      % Test,
    Munit.catsEffect % Test,
  ),
  addCompilerPlugin(Dependencies.Plugin.Compiler.kindProjector),
  addCompilerPlugin(Dependencies.Plugin.Compiler.betterMonadicFor),
)


lazy val client = (project in file("client"))
  .enablePlugins(ScalaJSPlugin, ScalaJSWeb)
  .dependsOn(shared)
  .settings(
    name := "client",

    scalaVersion := "2.13.6",
    scalaJSUseMainModuleInitializer := true,

    libraryDependencies += junit % Test,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0",
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.9.1",
  )


lazy val shared = (project in file("shared"))
  .aggregate(skeleton)
  .settings(
    name := "shared",
    scalaVersion := "2.13.6",

    libraryDependencies += junit % Test,
    libraryDependencies += scalatags,
  )

lazy val skeleton = (project in file("skeleton"))
  .settings(
    name := "skeleton",
    scalaVersion := "2.13.6",

    libraryDependencies += junit % Test,
    libraryDependencies += scalatags,
  )





lazy val jsPipe = taskKey[Unit]("Copy compiled js files into server")
jsPipe := {
  println("Copying js and css files to shared assets...")
  copyFile(
    "./client/target/scala-2.13/client-fastopt/main.js",
    "./shared/assets/js/main.js"
  )
  copyFile(
    "./client/target/scala-2.13/client-fastopt/main.js.map",
    "./shared/assets/js/main.js.map"
  )
  copyFile(
    "./client/css/main.css",
    "./shared/assets/css/main.css"
  )
}

lazy val compileOthers = taskKey[Unit]("Compile server and client")
compileOthers := Def.sequential(
  (shared/Compile/compile),
  (client/Compile/fastLinkJS),
  (server/Compile/compile),
  jsPipe,
).value
