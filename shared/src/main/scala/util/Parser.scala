package blog.util

trait Parser[F[_], Result] {
  def parse(s: String): F[Result]
}


object Parser:

  // given JsonCoding[]

end Parser
