package blog.skeleton.compiler

import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import blog.core


trait Compiler[F[_]: Monad, Input, Output]
  extends core.Eval[F, Input, Output] {
  
  def compile(s: Input): F[Output] =
    s.eval
}








