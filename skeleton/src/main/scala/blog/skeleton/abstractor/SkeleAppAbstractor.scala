package blog.skeleton.abstractor

import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import blog.skeleton.Exprs.SkeleExpr



class SkeleAppAbstractor[F[_]]
  (using err: MonadError[F, Throwable])
  extends Abstractor[[A] =>> String => F[A], SkeleExpr, SkeleExpr]:
  
  import blog.skeleton.Exprs.SkeleExpr.*
  override def extract(expr: SkeleExpr): String => F[SkeleExpr] = name => {
    expr match
      case App(Var(s), xs) if s == name => expr.pure
      case App(f, xs) => 
        val error: F[SkeleExpr] = 
          err.raiseError(
            blog.Error(s"Error while abstracting $name component.")
          )
        xs.foldLeft(error) {
          (acc, x) =>
            extract(x)(name) // we need to check if succeed...
        }
      case _ => err.raiseError(blog.Error(s""))
  }
end SkeleAppAbstractor
