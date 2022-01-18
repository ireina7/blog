package blog.util

import blog.*
import blog.util.*
import cats.syntax.traverse


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
      override def run() = {
        val result = mach.value.unsafeRunSync()
        result
      }





  /**
   * Final effect used for the whole blog project.
   * @tparam F the next effect
   * @tparam Env the injection environment
   * @tparam A the output type
  */
  type Injection[F[_], Env, A] = Env ?=> F[A]

  given [Env, F[_]: Functor]: 
    Functor[[A] =>> Injection[F, Env, A]] with
    override def map[A, B](ma: Injection[F, Env, A])(f: A => B) = env ?=> {
      ma(using env).map(f)
    }
  

  given [Env, F[_]](using app: Applicative[F]): 
    Applicative[[A] =>> Injection[F, Env, A]] with {
    override def pure[A](a: A) = app.pure(a)
    override def ap[A, B](ff: Injection[F, Env, A => B])(fa: Injection[F, Env, A]) = env ?=> {
      app.ap(ff(using env))(fa(using env))
    }
  }

  given [Env, F[_]: Monad](using app: Applicative[[A] =>> Injection[F, Env, A]]):
    Monad[[A] =>> Injection[F, Env, A]] with {
    export app.pure
    override def flatMap[A, B](fa: Injection[F, Env, A])(f: A => Injection[F, Env, B]) = env ?=> {
      fa(using env).flatMap(a => f(a)(using env))
    }
    override def tailRecM[A, B]
      (a: A)(f: A => Injection[F, Env, Either[A, B]]): Injection[F, Env, B] = {
      flatMap(f(a)) {
        case Left(a) => tailRecM(a)(f)
        case Right(b) => pure(b)
      }
    }
  }

  given [F[_]: Traverse, Env]:
    Traverse[[A] =>> Injection[F, Env, A]] with {

      // def traverse()
      override def foldLeft[A, B]
        (fa: Injection[F, Env, A], b: B)
        (f: (B, A) => B): B = {
        ???
      }
      override def foldRight[A, B]
        (fa: Injection[F, Env, A], lb: cats.Eval[B])
        (f: (A, cats.Eval[B]) => cats.Eval[B])
        : cats.Eval[B] = {
        ???
      }

      override def traverse[G[_], A, B]
        (fa: Injection[F, Env, A])
        (f: A => G[B])
        (implicit evidence$1: cats.Applicative[G])
        : G[Injection[F, Env, B]] = {
        ???
      }
  }

  
  given [F[_], Env](using fio: FileIO[F, String, String]):
    FileIO[[A] =>> Injection[F, Env, A], String, String] with {
    override def readFile(path: String) = 
      fio.readFile(path)
    override def writeFile(path: String, content: String) = 
      fio.writeFile(path, content)
  }
  

  given [F[_], Env](using fp: Parser[F, page.Index]):
    Parser[[A] =>> Injection[F, Env, A], page.Index] with
    override def parse(s: String) = fp.parse(s)
  

  given [F[_], Env](using F: Runnable[F], env: Env):
    Runnable[[A] =>> Injection[F, Env, A]] with {

    extension [T](mach: Injection[F, Env, T]) 
      override def run() = {
        /** 
         * If we write `mach.run()` or `mach(using env).run()` here,
         * We will get into trouble since above 2 expressions will be
         * reconstructed by scala compiler as `(env ?=> mach(using env)).run()`
         * and finally cause stackoverflow :(
         */
        F.run(mach(using env))() // We have to explicitly refer to the exact version of run.
      }
  }


end Effect
