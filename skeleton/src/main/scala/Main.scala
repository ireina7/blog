package blog.skeleton

import cats.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.effect.IO
import cats.data.*

import blog.core.*
// import blog.FileIOString
import blog.skeleton.Exprs.SkeleExpr



trait Skeleton[F[_]: Monad, Output]
  (using
    fileIO: FileIOString[F],
    parser: Parser[F, SkeleExpr],
    evalIt: blog.core.Eval[F, SkeleExpr, Output],
  ):
  import fileIO.{ readFile, writeFile }
  import parser.parse

  def register(path: String): F[Unit] = 
    for
      text <- readFile(path)
      tree <- parse(text)
      html <- tree.eval
      _    <- writeFile(path, html.toString)
    yield ()

  def testReg(path: String) = 
    for
      text <- readFile(path)
      tree <- parse(text)
      html <- tree.eval
    yield html

end Skeleton








object Skeleton:
  
  def register[F[_]: Monad, Output]
    (path: String)
    (using skele: Skeleton[F, Output]): F[Unit] = {
    
    skele.register(path)
  }

  def main(args: Array[String]): Unit =
    import Effect.{*, given}
    import MarkDownEvaluator.given
    import blog.static.given

    given blog.Configuration = blog.Configuration.staticBlog
    given MarkDownEvaluator.Environment =
      MarkDownEvaluator.Environment.predef
    
    type GenEffect[A] = Injection[IOErr, blog.Configuration, A]
    type Effect[A] = //blog.Configuration ?=> 
      MarkDownEvaluator.Skele[
        IOErr, 
        A
      ]

    // given blog.core.Eval[Effect, Exprs.SkeleExpr, blog.HtmlText] = 
    //   MarkDownEvaluator.evalMarkDown[Effect]
    // println(x.eval)
    // summon[blog.core.Eval[Effect, Exprs.SkeleExpr, blog.HtmlText]]

    def testCombination[F[_]: Monad, EnvA, EnvB]
      (skeleton : Skeleton [[B] =>> EnvB ?=> F[B], blog.HtmlText])
      (generator: Generator[[A] =>> EnvA ?=> F[A]])
      (path: String)
      (using
        envA: EnvA,
        envB: EnvB,
      ) = {

      for
        html <- skeleton.testReg(path)
      yield generator.generateHtml(html)
    }


    val generator = summon[Generator[GenEffect]]
    val skeleton = new Skeleton[Effect, blog.HtmlText] {}
    val result = 
      testCombination
        [IOErr, blog.Configuration, MarkDownEvaluator.Environment]
        (skeleton)(generator)("./skeleton/scripts/welcome.skele")
    // val result = skeleton
    //   .register("./skeleton/scripts/welcome.skele")
    //   .run()
    
    result.run() match
      case Left(err) => println(s"$err")
      case Right(ok) => println(s"Ok: $ok")

  end main
    
end Skeleton

