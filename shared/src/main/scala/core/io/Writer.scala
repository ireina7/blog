package blog.core

import cats.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*


trait Writer[F[_]: Monad, Path, -Input] {
  def write(x: Input)(path: Path): F[Unit]
}



object Writer:

  def write[F[_]: Monad, Path, Input]
    (x: Input)(path: Path)
    (using writer: Writer[F, Path, Input]): F[Unit] =
    writer.write(x)(path)

  given [F[_]: Monad, Path, Input](using
    fileIO: FileIO[F, Path, String],
    decoder: Decoder[F, Input],
  )
  : Writer[F, Path, Input] = new Writer {
    def write(x: Input)(path: Path): F[Unit] = {
      for {
        s <- decoder.decode(x)
        _ <- fileIO.writeFile(path, s)
      } yield ()
    }
  }

end Writer
