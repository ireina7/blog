
import sbt._


object V {

  // Scala versions
  val scala3             = "3.0.1"
  val scala2             = "2.13.6" 

  // Libraries
  val junit              = "0.11"
  val scalaParCollection = "1.0.3"
  val rxScala            = "0.27.0"
  val akka               = "2.6.15"
  val http4s             = "0.21.24"
  val logBack            = "1.2.3"
  val doobie             = "0.12.1"
  val circe              = "0.13.0"
  val munit              = "0.7.20"
  val munitCatsEffect    = "0.13.0"
  val scalatags          = "0.9.1"
  val scalaJsDom         = "1.1.0"

  // Plugins
  val kindProjector      = "0.10.3"
  val betterMonadicFor   = "0.3.1"
}


object Dependencies {
  
  object Library {
    val junit              = "com.novocode"           %  "junit-interface"            % V.junit
    val logBack            = "ch.qos.logback"         %  "logback-classic"            % V.logBack
    val scalaParCollection = "org.scala-lang.modules" %% "scala-parallel-collections" % V.scalaParCollection
    val rxScala            = "io.reactivex"           %% "rxscala"                    % V.rxScala
    val akka               = "com.typesafe.akka"      %% "akka-actor-typed"           % V.akka
    val circe              = "io.circe"               %% "circe-generic"              % V.circe
    val scalatags          = "com.lihaoyi"            %% "scalatags"                  % V.scalatags

    object Http4s {
      val server    = "org.http4s"      %% "http4s-blaze-server" % V.http4s
      val client    = "org.http4s"      %% "http4s-blaze-client" % V.http4s
      val circe     = "org.http4s"      %% "http4s-circe"        % V.http4s
      val dsl       = "org.http4s"      %% "http4s-dsl"          % V.http4s
      val scalatags = "org.http4s"      %% "http4s-scalatags"    % V.http4s
    }
    object Doobie {
      val core     = "org.tpolecat" %% "doobie-core"     % V.doobie
      val postgres = "org.tpolecat" %% "doobie-postgres" % V.doobie
      val specs2   = "org.tpolecat" %% "doobie-specs2"   % V.doobie
    }
    object Munit {
      val munit      = "org.scalameta" %% "munit"               % V.munit
      val catsEffect = "org.typelevel" %% "munit-cats-effect-2" % V.munitCatsEffect
    }
  }

  object Plugin {
    object Compiler {
      val kindProjector    = "org.typelevel" %% "kind-projector"     % V.kindProjector
      val betterMonadicFor = "com.olegpy"    %% "better-monadic-for" % V.betterMonadicFor
    }
    object Sbt {
      val sbt                 = "com.typesafe.play"        % "sbt-plugin"                % "2.8.8"
      val giter8Scaffold      = "org.foundweekends.giter8" % "sbt-giter8-scaffold"       % "0.11.0"
      val eclipse             = "com.typesafe.sbteclipse"  % "sbteclipse-plugin"         % "5.2.4"
      val scalajs             = "org.scala-js"             % "sbt-scalajs"               % "1.6.0"
      val webScalajs          = "com.vmunier"              % "sbt-web-scalajs"           % "1.2.0"
      val gzip                = "com.typesafe.sbt"         % "sbt-gzip"                  % "1.0.2"
      val digest              = "com.typesafe.sbt"         % "sbt-digest"                % "1.1.4"
      val scalajsCrossProject = "org.portable-scala"       % "sbt-scalajs-crossproject"  % "1.1.0"
    }
  }
}

object Operations {

  def copyFile(from: String, to: String): Unit = {
    import java.io.{ File,FileInputStream, FileOutputStream }
    
    val a = new File(from)
    val b = new File(to)
    new FileOutputStream(b)
      .getChannel()
      .transferFrom(new FileInputStream(a) getChannel, 0, Long.MaxValue)
  }
}
