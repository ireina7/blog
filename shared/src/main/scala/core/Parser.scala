package blog.core

trait Parser[F[_], Result] {
  def parse(s: String): F[Result]
}


object Parser:

  // given JsonCoding[]
  export blog.core.Effect.given Parser[?, ?]

  given fromDecoder
    [F[_], Result](using decoder: Decoder[F, Result])
    : Parser[F, Result] with
    override def parse(s: String) = 
      decoder.decode(s)
  // end given

end Parser
