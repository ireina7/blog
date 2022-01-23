package blog.shared

import cats.*
import cats.effect.*
import blog.core.Effect.*
import blog.core.Effect.given
import blog.static.given


import blog.core.*
def testGenerator[Eff[_]: Monad: Runnable]
  (using 
    generator: Generator[Eff],
    console: Console[Eff],
  ) = {
  import cats.syntax.flatMap.catsSyntaxFlatMapOps
  console.log("Generating static html file...") >>
  generator.generateIndexPage
}


@main def main(): Unit = {

  given blog.Configuration = blog.Configuration.staticBlog
  type Effect[A] = Injection[IOErr, blog.Configuration, A]

  val console = summon[Console[Effect]]
  def log(s: String) = console.log(s).run()
  log("Testing shared module...")
  // val html = blog.static.Generator.generateHtml()
  // println(html.toString)
  
  val exe = testGenerator[Effect]
  val result = exe.run()
  result match
    case Left(error) => log(s"Error while running generator: $error")
    case _ => log(s"Ok generated index page.")
  
}