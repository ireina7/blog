package blog.shared

import cats.*
import cats.effect.*


@main def main(): Unit = {

  println("blog> Testing shared module...")
  // val html = blog.static.Generator.generateHtml()
  // println(html.toString)

  println("blog> Generating static html file...")
  val exe = blog.static.Generator.writeHtmlToFile[IO]("./shared/public/index.html")
  exe.unsafeRunSync()
}