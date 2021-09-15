package blog.main

import cats.*
import cats.effect.{
  IO,
  IOApp,
  ExitCode
}


object Blog extends IOApp {

  def run(args: List[String]): IO[ExitCode] = IO {

    println("blog> Initializing...")
    // blog.static.Generator.writeHtmlToFile("./shared/public/index.html")

  }.as(ExitCode.Success)
}