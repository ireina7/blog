package blog

import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*
// import cats.syntax.applicative.*


trait Console[F[_]](using ev: Monad[F]) {
  
  def print(s: String): F[Unit]
  def println(s: String): F[Unit] = for {
    _ <- print(s)
    _ <- print("\n")
  } yield ()
  def readLine(): F[String]
  def readChar(): F[Char]
}


object Console {
  import cats.*

  // Dirty one
  given Console[Id] with {
    def print(s: String) = scala.Console.print(s)
    def readLine() = scala.io.StdIn.readLine()
    def readChar() = scala.io.StdIn.readChar()
  }
}