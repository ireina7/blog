ThisBuild / organization := "blog"
ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := V.scala3

import Dependencies._
import Library._
import Operations._


lazy val blog = (project in file("."))
  .aggregate(server, client, shared, skeleton)
  // .dependsOn(server, client, shared)
  .settings(
    name := "blog",
    scalaVersion := V.scala3,
    scalacOptions += "-source:future",
    scalacOptions += "-Ykind-projector:underscores",

    libraryDependencies ++= Seq(
      catsCore,
      catsFree,
      catsLaws,
      catsEffect,

      junit      % Test,
      scalaCheck % Test,
    ),

    
    Compile/compile := (Compile/compile).dependsOn(compileOthers).value,
    // Compile/runner := (Compile/runner).dependsOn(server/Compile/runner).value

    // update/evictionWarningOptions :=
    //   EvictionWarningOptions.default
    //     .withWarnScalaVersionEviction(false)
        // .withWarnTransitiveEvictions(false)
        // .withWarnDirectEvictions(false)
  )

lazy val server = (project in file("server"))
  .dependsOn(shared)
  .settings(
    name := "server",
    scalaVersion := V.scala3,//"2.13.6",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      // "-language:higherKinds",
      "-language:postfixOps",
      "-feature",
      "-Xfatal-warnings",
      "-source:future",
      "-Ykind-projector:underscores",
    ),
    libraryDependencies += logBack,
    libraryDependencies += junit,
    libraryDependencies ++= Seq(

      Circe.generic,
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
    ).map(_.cross(CrossVersion.for3Use2_13)),
  // addCompilerPlugin(Dependencies.Plugin.Compiler.kindProjector),
  // addCompilerPlugin(Dependencies.Plugin.Compiler.betterMonadicFor),
  )


lazy val client = (project in file("client"))
  .enablePlugins(ScalaJSPlugin, ScalaJSWeb)
  .dependsOn(shared)
  .settings(
    name := "client",

    scalaVersion := V.scala3,
    scalaJSUseMainModuleInitializer := true,

    libraryDependencies += junit % Test,
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "1.1.0",
      "com.lihaoyi" %%% "scalatags" % "0.9.1",
    ).map(_.cross(CrossVersion.for3Use2_13)),
  )

lazy val shared = (project in file("shared"))
  // .aggregate(skeleton)
  .settings(
    name := "shared",
    scalaVersion := V.scala3,
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      // "-language:higherKinds",
      "-language:postfixOps",
      "-feature",
      "-Xfatal-warnings",
      "-source:future",
      "-Ykind-projector:underscores",
    ),

    libraryDependencies ++= Seq(
      catsCore,
      catsEffect,
      scalatags,
      Circe.core,
      Circe.generic,
      Circe.parser,
    ).map(_.cross(CrossVersion.for3Use2_13)),
    libraryDependencies += junit % Test,
    // libraryDependencies += scalatags.cross(CrossVersion.for3Use2_13),
  )

lazy val skeleton = (project in file("skeleton"))
  .dependsOn(shared)
  .settings(
    name := "skeleton",
    scalaVersion := V.scala3,

    libraryDependencies += junit % Test,
    libraryDependencies ++= Seq(
      scalatags,
      scalaParser,
      scalaCheck % Test,
      catsLaws % Test,
    ).map(_.cross(CrossVersion.for3Use2_13))
  )





lazy val jsPipe = taskKey[Unit]("Copy compiled js files into server")
jsPipe := {
  val jsSourceFolder = "target/scala-3.1.0/client-fastopt"
  println("blog> Copying js and css files to shared assets...")
  copyFile(
    s"./client/$jsSourceFolder/main.js",
    "./shared/assets/js/main.js"
  )
  copyFile(
    s"./client/$jsSourceFolder/main.js.map",
    "./shared/assets/js/main.js.map"
  )
  copyFile(
    "./client/css/main.css",
    "./shared/assets/css/main.css"
  )
  println(s"blog> Refreshing assets folder...")
  copyFolder(
    "./shared/assets",
    "./shared/public/assets"
  )
  copyFolder(
    "./shared/assets",
    "./shared/staticPackage/assets"
  )
  copyFolder(
    "./skeleton/scripts",
    "./shared/staticPackage/scripts"
  )
}

lazy val compileOthers = taskKey[Unit]("Compile server and client")
compileOthers := Def.sequential(
  shared/Compile/compile,
  client/Compile/fastLinkJS,
  server/Compile/compile,
  jsPipe,
).value




