package blog.core

trait Parser[F[_], Result] {
  def parse(s: String): F[Result]
}


object Parser:

  // given JsonCoding[]
  export blog.core.Effect.given Parser[?, ?]

end Parser
