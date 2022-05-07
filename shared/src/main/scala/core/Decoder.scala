package blog.core

trait Decoder[F[_], Input] {
  def decode(x: Input): F[String]
}


object Decoder:
  def decode[F[_], Input](x: Input)
    (using decoder: Decoder[F, Input]) =
    decoder.decode(x)


  import blog.page
  import cats.*
  import cats.syntax.flatMap.*
  import cats.syntax.functor.*
  import cats.syntax.applicative.*
  
  given given_indexDecoder[F[_]: Applicative]
    : Decoder[F, page.Index] = new Decoder {
    
    def toJson(index: page.Index): F[String] = {
      import io.circe.syntax.*
      import io.circe.{ Decoder, Encoder, Json}, io.circe.generic.auto.*

      given encodeEvent: Encoder[page.Item] = new Encoder[page.Item] {
        final def apply(a: page.Item): Json = Json.obj(
          ("title",  Json.fromString(a.title)),
          ("link",   Json.fromString(a.link)),
          ("author", Json.fromString(a.author)),
          ("date",   Json.fromString(a.date)),
          ("view",   Json.fromString(a.view)),
        )
      }
      index.asJson.toString.pure
    }
    
    override def decode(idx: page.Index) = 
      toJson(idx)
  }


  given given_htmlDecoder[F[_]: Applicative]
    : Decoder[F, blog.HtmlText] = _.toString.pure
  
end Decoder




