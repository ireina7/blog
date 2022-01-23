package blog.core

import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*
// import cats.syntax.applicative.*


trait Console[F[_]](using ev: Monad[F]) {
  
  def print(s: String): F[Unit]
  def println(s: String): F[Unit] = print(s) >> print("\n")
  def log(msg: String): F[Unit] = println(msg)
  def readLine(): F[String]
  def readChar(): F[Char]
}


object Console {
  import cats.*
  import cats.effect.*

  // Dirty one
  given Console[cats.Id] with {
    def print(s: String) = scala.Console.print(s)
    def readLine() = scala.io.StdIn.readLine()
    def readChar() = scala.io.StdIn.readChar()
  }

  given Console[IO] with {
    def print(s: String) = IO { summon[Console[Id]].print(s) }
    def readLine() = IO { summon[Console[Id]].readLine() }
    def readChar() = IO { summon[Console[Id]].readChar() }
  }

  export blog.core.Effect.given Console[?]
}