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



trait Skeleton
  [
    F[_]: Monad, 
    // Output,
    MarkDownEnv,
    ExprEnv,
    GenEnv,
  ]
  (using
    fileIO: FileIOString[F],
    parser: Parser[F, SkeleExpr],
    evalMarkDown: blog.core.Eval[[A] =>> MarkDownEnv ?=> F[A], SkeleExpr, blog.HtmlText],
    evalExpr: blog.core.Eval[[A] =>> ExprEnv ?=> F[A], SkeleExpr, SkeleExpr],
    generator: Generator[[A] =>> GenEnv ?=> F[A]],
  ):
  import fileIO.{ readFile, writeFile }
  import parser.parse

  def register(path: String, tag: List[String] = Nil)
    (using ExprEnv, MarkDownEnv, GenEnv): F[Unit] = 
    for
      text <- readFile(path)
      tree <- parse(text)
      expr <- evalExpr.eval(tree)
      html <- evalMarkDown.eval(expr)
      _    <- {
              println(html)
              fileIO.writeFile(
                s"${blog.Path.staticPackage}/welcome.html",
                generator.generateHtml(html).toString
              )
              }
    yield ()

end Skeleton








object Skeleton:

  def main(args: Array[String]): Unit =
    import Effect.{*, given}
    import MarkDownEvaluator.given
    import PreMarkDownExprEvaluator.given
    import blog.static.given

    given blog.Configuration = blog.Configuration.staticBlog
    given a: MarkDownEvaluator.Environment =
      MarkDownEvaluator.Environment.predef
    given b: PreMarkDownExprEvaluator.Environment = 
      PreMarkDownExprEvaluator.Environment.predefForMarkDown


    // given generator = summon[Generator[GenEffect]]
    val skeleton: Skeleton[
      IOErr,
      // blog.HtmlText,
      MarkDownEvaluator.Environment,
      PreMarkDownExprEvaluator.Environment,
      blog.Configuration,
    ] = 
      new Skeleton {}
    
    val exe = 
      skeleton.register(
        "./skeleton/scripts/welcome.skele", 
        List("default")
      )
    
    exe.run() match
      case Left(err) => println(s"$err")
      case Right(ok) => println(s"Ok: $ok")

  end main
    
end Skeleton

