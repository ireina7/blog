package util

trait JsonCoding[F[_], Json] {
  def encode(s: String): F[Json]
  def decode(json: Json): F[String]
}


object JsonCoding:

  // given JsonCoding[]

end JsonCoding
