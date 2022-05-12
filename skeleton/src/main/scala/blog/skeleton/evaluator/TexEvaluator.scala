package blog.skeleton.evaluator

import cats.{
  Monad,
  Applicative,
  Traverse,
}
import blog.core.Eval
import blog.skeleton.Exprs.SkeleExpr.*
import blog.skeleton.Exprs.*

import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.traverse.*
import cats.MonadError
import blog.skeleton.evaluator.EvalSkeleExpr


object TexEvaluator:
  import blog.core.Effect.{*, given}
  import scala.collection.mutable

  type Environment = SkeleExpr.Env
  type Skele[F[_], A] = Injection[F, Environment, A]
  type Tex = String

  given given_evalTex[F[_]](using err: MonadError[F, Throwable])
    : EvalSkeleExpr[[A] =>> Skele[F, A], Tex] =
    ???

  // def evalTex[F[_]]
  //   (using errDsl: MonadError[F, Throwable])
  //   : EvalSkeleExpr[[A] =>> Skele[F, A], Tex] = 
  //   new EvalSkeleExpr:
    
  //     override def variable(name: String) = env ?=> 
  //       env.get(name) match
  //         case Some(x) => x.toString.pure
  //         case None => errDsl.raiseError(blog.Error(s"Markdown: Variable not found: $name"))
    
  //   end new
end TexEvaluator
