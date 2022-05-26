package blog.skeleton.compiler

import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import blog.core


trait Compiler[F[_]: Monad, Output]
  extends core.Eval[F, String, Output] {
  
  def compile(s: String): F[Output] =
    s.eval
}








