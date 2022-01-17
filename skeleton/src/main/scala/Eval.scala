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



type Env[A] = scala.collection.mutable.Map[String, A]
type SkeleEnv[F[_], Env, A] = Env ?=> F[A]
// type SkeleValueEnv[A] = SkeleEnv[Option, Env[A], A]


given [F[_], Env](using app: Applicative[F]): 
  Applicative[[A] =>> SkeleEnv[F, Env, A]] with
  def pure[A](a: A) = env ?=> app.pure(a)
  def ap[A, B](f: SkeleEnv[F, Env, A => B])(ma: SkeleEnv[F, Env, A]): SkeleEnv[F, Env, B] = 
    env ?=> {
      app.ap(f(using env))(ma(using env))
    }
end given


given [F[_], Env](using traversing: Traverse[F]):
  Traverse[[A] =>> SkeleEnv[F, Env, A]] with
  override def foldLeft[A, B](fa: SkeleEnv[F, Env, A], b: B)(f: (B, A) => B): B = ???
  override def foldRight[A, B]
    (fa: SkeleEnv[F, Env, A], lb: cats.Eval[B])
      (f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = ???
  override def traverse[G[_], A, B]
    (fa: SkeleEnv[F, Env, A])(f: A => G[B])
    (using appG: Applicative[G]): G[SkeleEnv[F, Env, B]] = ???
end given



type SkeleValueEnv[A] = SkeleEnv[blog.Result, Env[SkeleExpr], A]
given EvalSkele[SkeleValueEnv, SkeleExpr] with
  def variable(name: String) = env ?=> {
    ???
  }
  def integer(i: Int) = Right(SkeleExpr.integer(i))
  def number(n: Double) = Right(SkeleExpr.number(n))
  def string(s: String) = Right(SkeleExpr.string(s))
  def list(xs: List[SkeleExpr]) = ???
  override def application(
    xs: SkeleValueEnv[List[SkeleExpr]]
  ) = ???




object Eval:
  import cats.catsInstancesForId
  def d = 0
  val s = summon[Applicative[[A] =>> SkeleValueEnv[A]]]
  val i = summon[Applicative[cats.Id]]

end Eval
