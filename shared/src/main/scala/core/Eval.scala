package blog.core

trait Eval[F[_], A, B] extends From[F, A, B] { self =>
  extension (a: A) def eval: F[B] = from(a)
  override def from(a: A): F[B] = a.eval
  def widen[A1 >: A, B1 <: B]: Eval[F, A1, B1] = new Eval {
    extension (a: A1) 
      override def eval: F[B1] = 
        ???
  }
}

object Eval:

  // given Eval[]

end Eval

