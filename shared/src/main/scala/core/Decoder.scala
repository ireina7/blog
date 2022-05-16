package blog.core

trait Decoder[F[_], Result] {
  def decode(s: String): F[Result]
}


object Decoder:

  def decode[F[_], Result](s: String)
    (using decoder: Decoder[F, Result]) =
    decoder.decode(s)

  given [F[_], Result](using parser: Parser[F, Result])
    : Decoder[F, Result] = new Decoder {
    
    override def decode(s: String) = parser.parse(s)
  }

end Decoder

