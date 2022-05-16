package blog.skeleton

import blog.core.*
import cats.*
import cats.data.*
import cats.effect.IO
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
// import blog.FileIOString
import blog.skeleton.Exprs.SkeleExpr
import blog.skeleton.parser
import blog.skeleton.evaluator
import scala.annotation.tailrec


class Repl[F[_]: Monad]
  (using 
    console: Console[F],
    parser: Parser[F, SkeleExpr],
    evaluator: blog.core.Eval[F, SkeleExpr, SkeleExpr],
  ):
  // @tailrec
  /** The main Repl loop
   * @return ()
  */
  final def loop(using conf: blog.Configuration): F[Unit] = 
    for
      _      <- console.print(conf.prompt)
      input  <- console.readLine()
      output <- input match 
        case "exit" => return ().pure
        case "help" => console.println("help message ") >> loop
        case _ => 
          for
            expr  <- parser.parse(input)
            value <- evaluator.eval(expr)
            _     <- console.println("...") >> loop
          yield ()
    yield ()
  end loop
end Repl


