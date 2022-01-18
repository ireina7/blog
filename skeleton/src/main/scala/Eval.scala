package blog.skeleton

import cats.{
  Monad,
  Applicative,
  Traverse,
}
import blog.util.Eval
import blog.skeleton.Exprs.SkeleExpr.*
import blog.skeleton.Exprs.*

import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.traverse.*


trait EvalSkele[F[_]
  : Applicative
  : Traverse
  , Value
] extends 
  Eval[F, SkeleExpr, Value], 
  Expr[F, Value]:
  def eval(expr: SkeleExpr): F[Value] = {
    expr match
      case Var(name) => variable(name)
      case Integer(i) => integer(i)
      case Num(n) => number(n)
      case Str(s) => string(s)
      case Lisp(xs) => xs match
        case Nil => ???
        case xs => application(xs.traverse(eval(_)))
      case _ => ???
  }

end EvalSkele






import blog.util.Effect.{*, given}
type Skele[F[_], A] = Injection[F, blog.BlogEnv[SkeleExpr], A]
given [F[_]: Monad: Traverse]:
  EvalSkele[[A] =>> Skele[F, A], blog.HtmlText] with {
  override def variable(name: String) = env ?=> {
    ???
  }
  override def integer(i: Int) = ???
  override def number(n: Double) = ???
  override def string(s: String) = ???
  override def list(xs: List[blog.HtmlText]) = ???
  override def application(
    xs: Skele[F, List[blog.HtmlText]]
  ) = ???
}












object Eval:
  import cats.catsInstancesForId
  def d = 0
  val i = summon[Applicative[cats.Id]]

end Eval
