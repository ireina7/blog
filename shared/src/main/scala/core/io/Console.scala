package blog.core

import scala.util.{Try, Success, Failure}
import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*
// import cats.syntax.applicative.*


trait Console[F[_]](using ev: Monad[F]) {
  
  def print(s: String): F[Unit]
  def println(s: String): F[Unit] = print(s"$s\n")
  def log(msg: String): F[Unit] = println(msg)
  def readLine(): F[String]
  def readChar(): F[Char]
}


object Console {
  import cats.*
  import cats.effect.*

  // Dirty one
  given rawConsole: Console[cats.Id] with {
    def print(s: String): Unit = scala.Console.print(s)
    def readLine(): String = scala.io.StdIn.readLine()
    def readChar(): Char = scala.io.StdIn.readChar()
  }

  given Console[IO] with {
    // def print(s: String) = IO { summon[Console[Id]].print(s) }
    // def readLine() = IO { summon[Console[Id]].readLine() }
    // def readChar() = IO { summon[Console[Id]].readChar() }
    override def print(s: String) = Try(rawConsole.print(s)) match
      case Success(_) => IO(())
      case Failure(e) => IO.raiseError(e)
    
    override def readChar() = Try(rawConsole.readChar()) match
      case Success(c) => IO(c)
      case Failure(e) => IO.raiseError(e)
    
    override def readLine() = Try(rawConsole.readLine()) match
      case Success(s) => IO(s)
      case Failure(e) => IO.raiseError(e)
    
  }

  export blog.core.Effect.given Console[?]
}