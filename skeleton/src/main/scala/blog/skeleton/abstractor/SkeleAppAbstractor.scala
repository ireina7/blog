package blog.skeleton.abstractor

import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import blog.skeleton.Exprs.SkeleExpr



final class SkeleAppAbstractor[F[_]]
  (using err: MonadError[F, Throwable])
  extends Abstractor[[A] =>> String => F[A], SkeleExpr, SkeleExpr]:
  
  import blog.skeleton.Exprs.SkeleExpr.*

  private def search(expr: SkeleExpr)(name: String): Option[SkeleExpr] =
    expr match
      case App(Var(s), xs) if s == name => Some(expr)
      case App(f, xs) => 
        // Let's try to be dirty
        for x <- xs do
          search(x)(name) match 
            case ok@Some(e) => return ok
            case None => {}
        return None
      case _ => None
  end search

  override def extract(expr: SkeleExpr): String => F[SkeleExpr] = name => {
    search(expr)(name) match
      case None => 
        err.raiseError(
          blog.Error(s"Error while abstracting: $name component Not found")
        )
      case Some(e) => e.pure
  }
  
end SkeleAppAbstractor
