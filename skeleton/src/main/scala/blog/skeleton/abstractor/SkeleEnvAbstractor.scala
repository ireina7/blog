package blog.skeleton.abstractor

import blog.core
import blog.skeleton.Exprs.SkeleExpr
import blog.skeleton.compiler.Compiler
import cats.MonadError


abstract class SkeleEnvAbstractor[F[_], Env] 
  extends Abstractor[[A] =>> Env ?=> F[A], SkeleExpr, blog.HtmlText]



object SkeleEnvAbstractor:
  import SkeleExpr.*
  import blog.skeleton.parser
  import blog.skeleton.evaluator
  import evaluator.MarkDownEvaluator

  
  given skeleEnvAbstractorF[F[_]](using 
    err: MonadError[F, Throwable],
    evaluator: core.Eval[[A] =>> MarkDownEvaluator.Environment ?=> F[A], SkeleExpr, blog.HtmlText],
  ): SkeleEnvAbstractor[F, MarkDownEvaluator.Environment] with
    override def extract(expr: SkeleExpr) = env ?=> 
      expr match 
        case Var(name) => evaluator.eval(expr)
        case _ => err.raiseError(blog.Error(s"SkeleEnvAbstractor error: found $expr"))
    end extract
  end skeleEnvAbstractorF

end SkeleEnvAbstractor
