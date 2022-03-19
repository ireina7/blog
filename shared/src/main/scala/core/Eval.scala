package blog.core

trait Eval[F[_], A, B] extends From[F, A, B] {
  extension (a: A) def eval: F[B] = from(a)
  override def from(a: A): F[B] = a.eval
}

object Eval:

  // given Eval[]

end Eval

