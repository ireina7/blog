package blog.core


trait From[F[_], A, B] {
  def from(a: A): F[B]
}


object From {

  import cats.*
  import cats.effect.*
  import io.circe.*
  import io.circe.generic.auto.*
  import io.circe.parser.*
  given itemDecoder: Decoder[blog.page.Item] = new Decoder[blog.page.Item]:
    def apply(c: HCursor) =
      for {
        id     <- c.downField("id"    ).as[Int]
        title  <- c.downField("title" ).as[String]
        link   <- c.downField("link"  ).as[String]
        author <- c.downField("author").as[String]
        date   <- c.downField("date"  ).as[String]
        view   <- c.downField("view"  ).as[String]
      } yield {
        blog.page.Item(id, title, link, author, date, view)
      }

  given parseJson[B](using decoder: io.circe.Decoder[B])
    : From[[T] =>> Either[Exception, T], String, B] with {
    
    def from(s: String): Either[Exception, B] = 
      io.circe.parser.decode(s)
  }

  given [B](using decoder: io.circe.Decoder[B])
    : From[[T] =>> IO[Either[Exception, T]], String, B] with {
    
    def from(s: String): IO[Either[Exception, B]] = 
      IO { io.circe.parser.decode(s) }
  }
}
