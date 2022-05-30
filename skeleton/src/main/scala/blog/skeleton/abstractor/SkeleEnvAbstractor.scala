package blog.skeleton.abstractor

import blog.core
import blog.skeleton.Exprs.SkeleExpr
import blog.skeleton.compiler.Compiler
import cats.MonadError


abstract class SkeleEnvAbstractor[F[_]] 
  extends Abstractor[F, SkeleExpr, blog.HtmlText]



object SkeleEnvAbstractor:
  import SkeleExpr.*
  import blog.core.Effect.*
  import blog.skeleton.parser
  import blog.skeleton.evaluator
  import evaluator.MarkDownEvaluator
  import evaluator.PreMarkDownExprEvaluator
  import cats.syntax.applicative.*
  import cats.syntax.flatMap.*
  import cats.syntax.functor.*

  
  given skeleEnvAbstractorF[F[_]](using 
    err: MonadError[F, Throwable],
    skeleExpr: core.Eval[
      [A] =>> Injection[F, PreMarkDownExprEvaluator.Environment, A], 
      SkeleExpr, 
      SkeleExpr,
    ],
    markDown: core.Eval[
      [A] =>> Injection[F, MarkDownEvaluator.Environment, A], 
      SkeleExpr, 
      blog.HtmlText
    ],
  ): SkeleEnvAbstractor[
    [A] =>> 
      // hard coded, just for now
      Injections[F, (PreMarkDownExprEvaluator.Environment, MarkDownEvaluator.Environment), A],
  ] with
    override def extract(expr: SkeleExpr) = env ?=> 
      expr match 
        case Var(name) => 
          for
            e <- skeleExpr.eval(expr)
            v <- markDown.eval(e)
          yield v
        case _ => err.raiseError(blog.Error(s"SkeleEnvAbstractor error: found $expr"))
    end extract
  end skeleEnvAbstractorF

end SkeleEnvAbstractor
