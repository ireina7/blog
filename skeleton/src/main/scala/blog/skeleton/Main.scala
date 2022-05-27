package blog.skeleton

import blog.core
import blog.core.Effect.*
import blog.core.Effect.given
import blog.core.*
import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import blog.skeleton.Exprs.SkeleExpr
import blog.skeleton.parser
import blog.skeleton.evaluator
import blog.skeleton.compiler.Compiler
import blog.skeleton.compiler.MarkDownCompiler




object Skeleton:

  def main(args: Array[String]): Unit =

    import Effect.{*, given}
    import Generator.given
    import parser.NaiveParser.given
    import evaluator.PreMarkDownExprEvaluator
    import evaluator.MarkDownEvaluator
    import MarkDownEvaluator.given
    import PreMarkDownExprEvaluator.given

    // given blog.Configuration = blog.Configuration.staticBlog
    given blog.Configuration = blog.Configuration.onlineBlog
    given markdownEnv: MarkDownEvaluator.Environment = 
      MarkDownEvaluator.Environment.predef
    given exprEnv: PreMarkDownExprEvaluator.Environment = 
      PreMarkDownExprEvaluator.Environment.predefForMarkDown

    val console = summon[Console[IOErr]]
    def log(s: String) = console.log(s).run()

    if(args.length < 3) {
      log("[error] Too few CLI argument. No command executed.")
      return ()
    }
    // println(args.toList)
    val filePath = args(1)
    val fileTitle = args(2)
    val blogNo: Option[Int] = 
      if args.length > 3 
      then Some(args(3).toInt) 
      else None

    // given generator = summon[Generator[GenEffect]]
    val skeleton: HtmlRegister[
      IOErr,
      MarkDownEvaluator.Environment,
      PreMarkDownExprEvaluator.Environment,
      blog.Configuration,
    ] = 
      new HtmlRegister
    
//    val link = summon[blog.Configuration].blogType.blogPath
    // println(link)
    // val exe = 
    //   skeleton.registerFile(filePath, fileTitle, blogNo)

    val exe = 
      skeleton.register(filePath, fileTitle, blogNo)
    
    exe.run() match
      case Left(err) => 
        println(s"[error] $err")
        println(s"[error] ${err.getStackTrace.mkString("Stack trace:\n\t", "\n\t", "\n")}")
      case Right(_)  => println(s"[info] Ok: $filePath registered.")

  end main
    
end Skeleton






