package blog.core

import cats.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*


trait Reader[F[_]: Monad, Path, Output] {
  def read(path: Path): F[Output]
}



object Reader:

  def read[F[_]: Monad, Path, Output]
    (path: Path)
    (using reader: Reader[F, Path, Output]) =
    reader.read(path)

  given [F[_]: Monad, Path, Output](using 
    fileIO: FileIO[F, Path, String],
    parser: Parser[F, Output],
  )
  : Reader[F, Path, Output] = new Reader {
    def read(path: Path): F[Output] = {
      for {
        raw <- fileIO.readFile(path)
        res <- parser.parse(raw)
      } yield res
    }
  }


end Reader


