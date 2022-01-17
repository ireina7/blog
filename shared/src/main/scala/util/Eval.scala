package blog.util

trait Eval[F[_], A, B] {
  def eval(a: A): F[B]
}

object Eval:

  // given Eval[]

end Eval

