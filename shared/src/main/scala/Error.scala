package blog


/**
 * The root of all errors
*/
class Error(val msg: String) extends Exception {
  final override def fillInStackTrace(): Throwable = this
  override def toString = 
    s"[Error]: $msg"
}

type Result[A] = Either[Throwable, A]
type MonadThrow[F[_]] = cats.MonadError[F, Throwable]


object Error {
  def error(msg: String) = Error(msg)

  given Conversion[io.circe.Error, blog.Error] with {
    override def apply(circeErr: io.circe.Error) =
      blog.Error(circeErr.getMessage)
  }
}
