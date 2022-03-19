package blog.skeleton

import cats.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.effect.IO
import cats.data.*

import blog.core.*
// import blog.FileIOString
import blog.skeleton.Exprs.SkeleExpr



trait Skeleton[F[_]: Monad, Output]
  (using
    fileIO: FileIOString[F],
    parser: Parser[F, SkeleExpr],
    evalIt: blog.core.Eval[F, SkeleExpr, Output],
  ):
  import fileIO.{ readFile, writeFile }
  import parser.parse

  def register(path: String): F[Unit] = 
    for
      text <- readFile(path)
      tree <- parse(text)
      html <- tree.eval
      _    <- writeFile(path, html.toString)
    yield ()

end Skeleton






object Skeleton:
  
  def register[F[_]: Monad, Output]
    (path: String)
    (using skele: Skeleton[F, Output]): F[Unit] = {
    
    skele.register(path)
  }
    
end Skeleton

