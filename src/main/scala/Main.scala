package blog.main

import cats.*
import cats.effect.*


object Blog extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    IO.println("Hello, blog!")
      .as(ExitCode.Success)
  }
}