package blog.core

trait Encoder[F[_], Input] {
  def encode(x: Input): F[String]
}


object Encoder:
  def encode[F[_], Input](x: Input)
    (using encoder: Encoder[F, Input]) =
    encoder.encode(x)


  import blog.page
  import cats.*
  import cats.syntax.flatMap.*
  import cats.syntax.functor.*
  import cats.syntax.applicative.*
  
  given given_indexEncoder[F[_]: Applicative]
    : Encoder[F, page.Index] = new Encoder {
    
    def toJson(index: page.Index): F[String] = {
      import io.circe
      import io.circe.syntax.*
      import io.circe.{ Decoder, Encoder, Json}, io.circe.generic.auto.*

      given encodeEvent: circe.Encoder[page.Item] = new circe.Encoder[page.Item] {
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
    
    override def encode(idx: page.Index) = 
      toJson(idx)
  }


  given given_htmlEncoder[F[_]: Applicative]
    : Encoder[F, blog.HtmlText] = _.toString.pure
  
end Encoder




