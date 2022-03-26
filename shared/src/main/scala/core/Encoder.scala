package blog.core

trait Encoder[F[_], Result] {
  def encode(s: String): F[Result]
}


object Encoder:

  def encode[F[_], Result](s: String)
    (using encoder: Encoder[F, Result]) =
    encoder.encode(s)

  given [F[_], Result](using parser: Parser[F, Result])
    : Encoder[F, Result] = new Encoder {
    
    override def encode(s: String) = parser.parse(s)
  }

end Encoder

