ThisBuild / organization := "blog"
ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := "2.13.6"

import Dependencies._
import Operations._


lazy val blog = (project in file("."))
    .aggregate(server, client, shared)
    // .dependsOn(server, client)
    .settings(
        name := "blog",
        scalaVersion := "2.13.6",

        libraryDependencies += junit,

        Compile/compile := (Compile/compile).dependsOn(compileOthers).value,
        // Compile/runner := (Compile/runner).dependsOn(server/Compile/runner).value
    )


val Http4sVersion = "0.21.24"
val CirceVersion = "0.13.0"
val MunitVersion = "0.7.20"
val MunitCatsEffectVersion = "0.13.0"
val LogbackVersion = "1.2.3"
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
            "org.http4s"      %% "http4s-blaze-server" % Http4sVersion,
            "org.http4s"      %% "http4s-blaze-client" % Http4sVersion,
            "org.http4s"      %% "http4s-circe"        % Http4sVersion,
            "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
            "org.http4s"      %% "http4s-scalatags"    % Http4sVersion,
            "io.circe"        %% "circe-generic"       % CirceVersion,
            "org.scalameta"   %% "munit"               % MunitVersion           % Test,
            "org.typelevel"   %% "munit-cats-effect-2" % MunitCatsEffectVersion % Test,
            "ch.qos.logback"  %  "logback-classic"     % LogbackVersion,
            "com.lihaoyi"     %% "scalatags"           % "0.9.1",
        ),
        addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3"),
        addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1"),
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
    .settings(
        name := "shared",

        scalaVersion := "2.13.6",

        libraryDependencies += junit % Test,
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
