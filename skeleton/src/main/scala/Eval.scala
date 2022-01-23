package blog.skeleton

import cats.{
  Monad,
  Applicative,
  Traverse,
}
import blog.core.Eval
import blog.skeleton.Exprs.SkeleExpr.*
import blog.skeleton.Exprs.*

import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.traverse.*
import cats.MonadError


trait EvalSkele[F[_]
  : Monad
  , Value
](using err: MonadError[F, Throwable]) extends 
  Eval[F, SkeleExpr, Value], 
  Expr[F, Value]:
  override def eval(expr: SkeleExpr): F[Value] = {
    expr match
      case Var(name)  => variable(name)
      case Integer(i) => integer(i)
      case Num(n)     => number(n)
      case Str(s)     => string(s)
      case Lisp(xs)   => ???
      case App(f, ps) => application(eval(f), ps.traverse(eval(_)))
      case _          => err.raiseError(
        Throwable(s"Evaluation error: Unknown expression: $expr")
      )
  }

end EvalSkele



import blog.core.Parser

given [F[_]: Monad, Value](using
    parser: Parser[F, SkeleExpr],
    evalSkele: EvalSkele[F, Value]
  ):
  Eval[F, String, Value] with {

  override def eval(s: String): F[Value] = {
    for
      exprs <- parser.parse(s)
      skele <- evalSkele.eval(exprs)
    yield skele
  }
}








import blog.core.Effect.{*, given}
import blog.core.Effect.given Monad[?]
import scala.collection.mutable

type Skele[F[_], A] = Injection[F, mutable.Map[String, SkeleExpr], A]


import cats.syntax.apply.*
given [F[_]: Monad](using err: MonadError[F, Throwable]):
  EvalSkele[[A] =>> Injection[F, mutable.Map[String, SkeleExpr], A], SkeleExpr] with {
  
  // import cats.syntax.apply.*
  given Conversion[SkeleExpr, F[SkeleExpr]] = _.pure
  override def variable(name: String) = env ?=> {
    env.get(name) match
      case Some(x) => err.pure(x)
      case None => err.raiseError(Throwable(s"Variable not found: $name"))
  }
  override def integer(i: Int) = Integer(i)
  override def number(n: Double) = Num(n)
  override def string(s: String) = Str(s)
  override def list(xs: List[SkeleExpr]) = Lisp(xs)
  override def application
    (f: Skele[F, SkeleExpr], xs: Skele[F, List[SkeleExpr]]): Skele[F, SkeleExpr] = {
    
    for
      ff <- f
      ys <- xs
      ans <- ff match
        case Closure(ps, expr, env) => {
          val pairs = (ps zip ys)
          val lefts = ps diff pairs.map(_._1)
          pairs.foreach {
            case (Var(name), v) => env += (name -> v)
            case _ => err.raiseError(
              Throwable(s"Application error: found $ff")
            )
          }
          if !lefts.isEmpty then
            Closure(lefts, expr, env).pure
          else
            eval(expr)(using env)
        }
        case _ => 
          err.raiseError(
            Throwable(s"Application error: found $ff")
          )
    yield ans
  }
}












object Eval:
  import cats.catsInstancesForId
  def d = 0
  val i = summon[Applicative[cats.Id]]

  import scala.collection.mutable
  import blog.skeleton.Exprs.*
  import SkeleExpr.*
  val env0: mutable.Map[String, SkeleExpr] = mutable.Map()
  env0 += (
    "+1" -> Closure(List(Var("x")), Integer(2), mutable.Map())
  )

end Eval
