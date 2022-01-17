package blog.util

import blog.*
import blog.util.*


trait Runnable[Effect[_]]:
  extension [A](mach: Effect[A]) def run(): blog.Result[A]





object Effect:
  
  /**
   * An example implementation of the effect F[_] inside readIndex
  */
  import cats.*
  import cats.syntax.flatMap.*
  import cats.syntax.functor.*
  import cats.syntax.applicative.*
  import cats.effect.IO
  import cats.data.*


  type IOErr[A] = EitherT[IO, Throwable, A]
  given FileIO[IOErr, String, String] with {
    def readFile(path: String) = {
      EitherT.rightT[IO, Throwable](summon[FileIOString[Id]].readFile(path))
    }
    def writeFile(path: String, content: String) = {
      EitherT.rightT[IO, Throwable](summon[FileIOString[Id]].writeFile(path, content))
    }
  }
  given Parser[IOErr, page.Index] with {
    def parse(s: String): IOErr[page.Index] = {
      import io.circe.*
      import io.circe.generic.auto.*
      import io.circe.parser.*
      given itemDecoder: Decoder[blog.page.Item] = new Decoder[blog.page.Item]:
        def apply(c: HCursor) =
          for {
            title  <- c.downField("title" ).as[String]
            link   <- c.downField("link"  ).as[String]
            author <- c.downField("author").as[String]
            date   <- c.downField("date"  ).as[String]
            view   <- c.downField("view"  ).as[String]
          } yield {
            blog.page.Item(title, link, author, date, view)
          }
      
      EitherT.fromEither(decode(s))
    }
  }

  given Runnable[IOErr] with
    extension [T](mach: IOErr[T]) 
      def run() = {
        val result = mach.value.unsafeRunSync()
        result match
          case Right(x) => Right(x)
          case Left(err) => Left(err)
      }





  /**
   * Final effect used for the whole blog project.
   * @tparam F the next effect
   * @tparam Env the injection environment
   * @tparam A the output type
  */
  type Mana[F[_], Env, A] = Env ?=> F[A]

  given [Env, F[_]: Functor]: 
    Functor[[A] =>> Mana[F, Env, A]] with
    def map[A, B](ma: Mana[F, Env, A])(f: A => B) = env ?=> {
      ma(using env).map(f)
    }

  given [Env, F[_]](using app: Applicative[F]): 
    Applicative[[A] =>> Mana[F, Env, A]] with
    def pure[A](a: A) = app.pure(a)
    def ap[A, B](ff: Mana[F, Env, A => B])(fa: Mana[F, Env, A]) = env ?=> {
      app.ap(ff(using env))(fa(using env))
    }

  given [Env, F[_]: Monad](using app: Applicative[[A] =>> Mana[F, Env, A]]):
    Monad[[A] =>> Mana[F, Env, A]] with
    export app.pure
    def flatMap[A, B](fa: Mana[F, Env, A])(f: A => Mana[F, Env, B]) = env ?=> {
      ???
    }
    def tailRecM[A, B](a: A)(f: A => Mana[F, Env, Either[A, B]]): Mana[F, Env, B] = env ?=> {
      ???
    }

  given [F[_], Env](using fio: FileIO[F, String, String]):
    FileIO[[A] =>> Mana[F, Env, A], String, String] with
    def readFile(path: String) = fio.readFile(path)
    def writeFile(path: String, content: String) = fio.writeFile(path, content)
  

  given [F[_], Env](using fp: Parser[F, page.Index]):
    Parser[[A] =>> Mana[F, Env, A], page.Index] with
    def parse(s: String) = fp.parse(s)

  given [F[_]: Runnable, Env](using env: Env):
    Runnable[[A] =>> Mana[F, Env, A]] with
    extension [T](mach: Mana[F, Env, T]) def run() = mach.run()


end Effect
