package blog.skeleton

import cats.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.effect.IO
import cats.data.*
import blog.FileIOString
import blog.skeleton.Exprs.SkeleExpr



trait Skeleton[F[_]: Monad]
  (using
    fileIO: FileIOString[F],
    // eval  : EvalSkele[F, SkeleExpr]
  ):

  def register(path: String): F[Unit] = {
    for {
      blog <- fileIO.readFile(path)
    } yield ()
  }

end Skeleton



object Skeleton:
  
  def register[F[_]: Monad](path: String)
    (using
      fileIO: FileIOString[F],
      eval  : EvalSkele[F, SkeleExpr]
    ): F[Unit] = {
    for {
      blog <- fileIO.readFile(path)
    } yield ()
  }
end Skeleton