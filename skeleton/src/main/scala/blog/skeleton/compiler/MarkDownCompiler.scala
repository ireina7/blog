package blog.skeleton.compiler

import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import blog.core
import blog.core.*
import blog.skeleton.Exprs.SkeleExpr
import blog.skeleton.parser
import blog.skeleton.evaluator
import blog.core.Effect.*
import blog.core.Effect.given



open class MarkDownCompiler[F[_]: Monad, Output, MarkEnv, ExprEnv]
  (using
    fileIO: FileIOString[F],
    parser: Parser[[A] =>> blog.Configuration ?=> F[A], SkeleExpr],
    evalMark: core.Eval[[A] =>> MarkEnv ?=> F[A], SkeleExpr, Output],
    evalExpr: core.Eval[[A] =>> ExprEnv ?=> F[A], SkeleExpr, SkeleExpr],
  ) extends blog.skeleton.compiler.Compiler[
    // [A] =>> 
    //   Injection[
    //     [A] =>> 
    //       Injection[
    //         [B] =>> MarkEnv ?=> F[B], 
    //         ExprEnv, 
    //         A
    //       ], 
    //     blog.Configuration, 
    //     A
    //   ], 
    [A] =>> Effect.Injections[F, (blog.Configuration, ExprEnv, MarkEnv), A],
    Output
  ]:
  import fileIO.readFile
  import parser.parse

  extension (text: String)
    override def eval = 
      for
        tree <- parse(text)
        expr <- evalExpr.eval(tree)
        html <- evalMark.eval(expr)
      yield html

  def compileFile(path: String)
    : Effect.Injections[F, (blog.Configuration, ExprEnv, MarkEnv), Output] =     
    for
      text <- readFile(path)
      html <- compile(text)
    yield html
  
end MarkDownCompiler


/** Various compilers for markdown compiling
 * Including:
 * - Html
*/
object MarkDownCompiler:
  import Effect.{*, given}
  import blog.Configuration.given
  import evaluator.PreMarkDownExprEvaluator
  import evaluator.MarkDownEvaluator

  def htmlCompilerIOErr(using conf: blog.Configuration) 
    : MarkDownCompiler[
      IOErr, 
      blog.HtmlText, 
      MarkDownEvaluator.Environment,
      PreMarkDownExprEvaluator.Environment
    ] = {
      
      import blog.skeleton.parser.NaiveParser.given
      import MarkDownEvaluator.given
      import PreMarkDownExprEvaluator.given

      new MarkDownCompiler
    }

  given given_HtmlCompilerIOErr(using conf: blog.Configuration)
    : MarkDownCompiler[
    IOErr, 
    blog.HtmlText, 
    MarkDownEvaluator.Environment,
    PreMarkDownExprEvaluator.Environment
  ] = htmlCompilerIOErr

end MarkDownCompiler



