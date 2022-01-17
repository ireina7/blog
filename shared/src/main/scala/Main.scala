package blog.shared

import cats.*
import cats.effect.*
import blog.util.Effect.*
import blog.util.Effect.given
import blog.static.given


import blog.util.Generator
def testGenerator[Eff[_]: blog.util.Runnable]
  (using generator: Generator[Eff]) = {
  
  val exe = generator.generateIndexPage
  exe.run()
}

@main def main(): Unit = {

  println("blog> Testing shared module...")
  // val html = blog.static.Generator.generateHtml()
  // println(html.toString)

  println("blog> Generating static html file...")
  val result = testGenerator[IOErr]
  result match
    case Left(error) => println(s"Error while running generator: $error")
    case _ => println(s"Ok generated index page.")
  
  // exe.unsafeRunSync()
}