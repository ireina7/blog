package blog.shared

import cats.*
import cats.effect.*
import blog.core.Effect.*
import blog.core.Effect.given


import blog.core.*
def testGenerator[Eff[_]: Monad: Runnable]
  (using 
    generator: BlogIndexGenerator[Eff],
    console: Console[Eff],
  ) = {
  import cats.syntax.flatMap.catsSyntaxFlatMapOps
  console.log("[blog] ", "Generating static html file...") >>
  generator.generateIndexPage
}


@main def main(): Unit = {

  import BlogIndexGenerator.given
  given blog.Configuration = blog.Configuration.onlineBlog
  type Effect[A] = Injection[IOErr, blog.Configuration, A]

  val console = summon[Console[Effect]]
  def log(s: String) = console.log("[blog] ", s).run()
  log("Testing shared module...")
  // val html = blog.static.Generator.generateHtml()
  // println(html.toString)
  
  val exe = testGenerator[Effect]
  val result = exe.run()
  result match
    case Left(err) => 
      log(s"Error while running generator: $err")
      log(s"${err.getStackTrace.toList.mkString("Stack trace:\n\t", "\n\t", "\n")}")
    case _ => log(s"Ok generated index page.")
  
}