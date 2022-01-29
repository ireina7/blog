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


trait EvalSkele[F[_], Value]
  (using err: MonadError[F, Throwable]) 
  extends 
  Eval[F, SkeleExpr, Value], 
  Expr[F, Value]:
  override def eval(expr: SkeleExpr): F[Value] = {
    expr match
      case Var(name)  => variable(name)
      case Integer(i) => integer(i)
      case Num(n)     => number(n)
      case Str(s)     => string(s)
      case Lisp(xs)   => xs.traverse(eval(_)).flatMap(list(_))
      case App(f, ps) => for {
        function   <- eval(f)
        parameters <- ps.traverse(eval(_))
        result     <- application(function, parameters)
      } yield result
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
import scala.collection.mutable

type Skele[F[_], A] = Injection[F, SkeleExpr.Env, A]



given [F[_]](using err: MonadError[F, Throwable]):
  EvalSkele[[A] =>> Skele[F, A], SkeleExpr] =
  new EvalSkele {
  
  import cats.syntax.apply.*
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
  override def lambda(ps: List[SkeleExpr], expr: SkeleExpr) = env ?=> {
    Closure(ps, expr, env).pure
  }
  override def application
    (f: SkeleExpr, xs: List[SkeleExpr]): Skele[F, SkeleExpr] = {
    
    f match
      case Closure(ps, expr, env) => {
        val pairs = (ps zip xs)
        val lefts = ps diff pairs.map(_._1)
        pairs.foreach {
          case (Var(name), v) => env += (name -> v)
          case (x, _) => err.raiseError(
            Throwable(s"""
              |Only application of variable is supported currently: 
              |found $x
            """.stripMargin)
          )
        }
        if !lefts.isEmpty then
          lambda(lefts, expr)(using env)
        else
          eval(expr)(using env)
      }
      case _ => 
        err.raiseError(
          Throwable(s"Application error: found $f")
        )
    end match
  }
}












object Eval:
  // import cats.catsInstancesForId
  // def d = 0
  // val i = summon[Applicative[cats.Id]]

  def SkelePredef[F[_]]
    (using parser: Parser[F, SkeleExpr])
    : SkeleExpr.Env = {
    import scala.collection.mutable
    import blog.skeleton.Exprs.*
    import SkeleExpr.*

    val env0: SkeleExpr.Env = mutable.Map()
    val env : SkeleExpr.Env = mutable.Map(
      "+1" -> Closure(List(Var("x")), Integer(2), env0)
    )
    env
  }

end Eval
