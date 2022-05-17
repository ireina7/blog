package blog.core

import blog.page
import blog.core.*
import cats.syntax.traverse
import scala.util.Success
import shapeless.Succ
import scala.languageFeature.existentials


trait Runnable[Effect[_]]:
  extension [A](mach: Effect[A]) 
    def run(): blog.Result[A]





object Effect:
  
  /**
   * An example implementation of the effect F[_] inside readIndex
  */
  import scala.util.{Try, Success, Failure}
  import cats.*
  import cats.syntax.flatMap.*
  import cats.syntax.functor.*
  import cats.syntax.applicative.*
  import cats.effect.IO
  import cats.data.*


  type IOErr[A] = EitherT[IO, Throwable, A]

  given [Path](using io: FileIO[IO, Path, String])
    : FileIO[IOErr, Path, String] with

    override def createFile(path: Path): IOErr[Unit] =
      EitherT(io.createFile(path).attempt)
    override def readFile(path: Path): IOErr[String] = 
      EitherT(io.readFile(path).attempt)
    override def writeFile(path: Path, content: String): IOErr[Unit] = 
      EitherT(io.writeFile(path, content).attempt)
    override def existFile(path: Path): IOErr[Boolean] =
      EitherT(io.existFile(path).attempt)
    override def createDirectory(path: Path): IOErr[Unit] = 
      EitherT(io.createDirectory(path).attempt)
    override def copyDirectory(from: Path, to: Path): IOErr[Unit] =
      EitherT(io.copyDirectory(from, to).attempt)
    override def existDirectory(path: Path): IOErr[Boolean] =
      EitherT(io.existDirectory(path).attempt)

  end given

  // given [Path](using rawIO: FileIO[Id, Path, String])
  //   : FileIO[IOErr, Path, String] with 

  //   override def createFile(path: Path): IOErr[Unit] = 
  //     EitherT.rightT[IO, Throwable]
  //       (Try(rawIO.createFile(path))).flatMap {
  //       case Success(_) => EitherT.rightT(())
  //       case Failure(e) => EitherT.leftT(e)
  //     }
  //   override def readFile(path: Path) = {
  //     // EitherT.rightT[IO, Throwable](rawIO.readFile(path))
  //     Try(rawIO.readFile(path)) match
  //       case Success(s) => EitherT.rightT(s)
  //       case Failure(e) => EitherT.leftT(e)
  //   }
  //   override def writeFile(path: Path, content: String) = {
  //     // EitherT.rightT[IO, Throwable](rawIO.writeFile(path, content))
  //     Try(rawIO.writeFile(path, content)) match
  //       case Success(_) => EitherT.rightT(())
  //       case Failure(e) => EitherT.leftT(e)
  //   }
  //   override def existFile(path: Path): IOErr[Boolean] = {
  //     Try(rawIO.existFile(path)) match
  //       case Success(b) => EitherT.rightT(b)
  //       case Failure(e) => EitherT.leftT(e)
  //   }

  //   override def createDirectory(path: Path): IOErr[Unit] = 
  //     Try(rawIO.createDirectory(path)) match
  //       case Success(_) => EitherT.rightT(())
  //       case Failure(e) => EitherT.leftT(e)

  //   override def copyDirectory(from: Path, to: Path): IOErr[Unit] =
  //     Try(rawIO.copyDirectory(from, to)) match
  //       case Success(_) => EitherT.rightT(())
  //       case Failure(e) => EitherT.leftT(e)
  // end given


  given (using ioio: Console[IO]): 
    Console[IOErr] with
    override def print(s: String) = EitherT(ioio.print(s).attempt)
    override def readChar() = EitherT(ioio.readChar().attempt)
    override def readLine() = EitherT(ioio.readLine().attempt)
  end given
  


  given Parser[IOErr, page.Index] with {
    def parse(s: String): IOErr[page.Index] = {
      import io.circe.*
      import io.circe.generic.auto.*
      import io.circe.parser.*
      given itemDecoder: Decoder[blog.page.Item] = new Decoder[blog.page.Item]:
        def apply(c: HCursor) =
          for 
            id     <- c.downField("id"    ).as[Int]
            title  <- c.downField("title" ).as[String]
            link   <- c.downField("link"  ).as[String]
            author <- c.downField("author").as[String]
            date   <- c.downField("date"  ).as[String]
            view   <- c.downField("view"  ).as[String]
          yield 
            blog.page.Item(id, title, link, author, date, view)
      
      EitherT.fromEither(decode(s))
    }
  }

  given Runnable[IOErr] with
    extension [T](mach: IOErr[T]) 
      override def run() = {
        val result = mach.value.unsafeRunSync()
        result
      }
  end given




  /**
   * Final effect used for the whole blog project.
   * @tparam F the next effect
   * @tparam Env the injection environment
   * @tparam A the output type
  */
  type Injection[F[_], Env, A] = 
    Env ?=> F[A]

  /*
  Env ?=> Injection[IOErr, Env1, A]
  Env ?=> Env1 ?=> IOErr[A]

  [A] =>> Injection[[B] =>> Injection[IOErr, env1, B], env0, A]
  */

  type AddEnv[T, Env2] = T match
    case Injection[f, env, a] => (Env2, env) ?=> f[a]

  given [Env, F[_]: Functor]: 
    Functor[[A] =>> Injection[F, Env, A]] with
    override def map[A, B](ma: Injection[F, Env, A])(f: A => B) = env ?=> {
      ma(using env).map(f)
    }
  

  given [Env, F[_]](using app: Applicative[F]): 
    Applicative[[A] =>> Injection[F, Env, A]] with {
    override def pure[A](a: A) = app.pure(a)
    override def ap[A, B]
      (ff: Injection[F, Env, A => B])
      (fa: Injection[F, Env, A]) = 
      env ?=> {
        app.ap(ff(using env))(fa(using env))
      }
  }

  given [F[_]: Monad, Env](using app: Applicative[[A] =>> Injection[F, Env, A]]):
    Monad[[A] =>> Injection[F, Env, A]] with {
    export app.pure
    override def flatMap[A, B]
      (fa: Injection[F, Env, A])
      (f: A => Injection[F, Env, B]) = 
      env ?=> {
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

  given [F[_]: Monad, Env](using 
    monad: Monad[[A] =>> Injection[F, Env, A]], 
    errF: MonadError[F, Throwable]
  ):
    MonadError[[A] =>> Injection[F, Env, A], Throwable] with {
    
    def handleErrorWith[A]
      (fa: Injection[F, Env, A])
      (f: Throwable => Injection[F, Env, A]): Injection[F, Env, A] = {
      fa // underwork!
    }
    def raiseError[A](e: Throwable): Injection[F, Env, A] = {
      println(s"raise error: $e")
      errF.raiseError(e)
    }
    
    export monad.*
  }


  given [F[_], Env, Path](using fio: FileIO[F, Path, String]):
    FileIO[[A] =>> Injection[F, Env, A], Path, String] with {

    override def createFile(path: Path): Injection[F, Env, Unit] =
      fio.createFile(path)
    override def readFile(path: Path) = 
      fio.readFile(path)
    override def writeFile(path: Path, content: String) = 
      fio.writeFile(path, content)
    override def existFile(path: Path) = 
      fio.existFile(path)
    
    override def createDirectory(path: Path) = 
      fio.createDirectory(path)
    override def copyDirectory(from: Path, to: Path) =
      fio.copyDirectory(from ,to)
    override def existDirectory(path: Path) =
      fio.existDirectory(path)
  }


  given given_console_config[F[_]: Monad](using 
    ioio: Console[F],
  ): Console[[A] =>> Injection[F, blog.Configuration, A]] with {
    
    override def print(s: String) = ioio.print(s)
    override def log(s: String) = env ?=> {
      val prompt = env.prompt
      ioio.println(s"$prompt$s")
    }
    override def readChar() = ioio.readChar()
    override def readLine() = ioio.readLine()
  }

  given given_console[F[_]: Monad, Env, Query, A](using 
    ioio: Console[F],
    envi: Environment[F, Env, Query, A],
  ): Console[[A] =>> Injection[F, Env, A]] with {
    
    override def print(s: String) = ioio.print(s)
    // override def log(s: String) = env ?=> {
    //   val prompt = env.prompt
    //   ioio.println(s"[$prompt] $s")
    // }
    override def readChar() = ioio.readChar()
    override def readLine() = ioio.readLine()
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
