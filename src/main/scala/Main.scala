package blog.main

import cats.*
import cats.effect.{
  IO,
  IOApp,
  ExitCode
}


// object Blog extends IOApp {

//   def run(args: List[String]): IO[ExitCode] = IO {

//     println("blog> Initializing...")
//     import blog.*
//     val j = new TestJava
//     j.show()
//     // blog.static.Generator.writeHtmlToFile("./shared/public/index.html")

//   }.as(ExitCode.Success)
// }


object Blog {

  def main(args: Array[String]): Unit = {

    println("blog> Initializing...")
    import blog.test.*
    val j = new TestJava
    j.show()
    // blog.static.Generator.writeHtmlToFile("./shared/public/index.html")

  }
}