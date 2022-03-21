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
import blog.skeleton.EvalSkeleExpr



object ExprEvaluator:

  import blog.core.Effect.{*, given}
  type Skele[F[_], A] = Injection[F, SkeleExpr.Env, A]

  given given_evalSkeleExpr[F[_]](using err: MonadError[F, Throwable])
    : EvalSkeleExpr[[A] =>> Skele[F, A], SkeleExpr] =
    evalSkeleExpr

  /** Implementation for Expression Evaluator (ExprEvaluator,EvalSkeleExpr)
   * evaluate: SkeleExpr => SkeleExpr
   * 
   * - Test imports:
      import blog.core.*
      import blog.core.given
      import Effect.*
      import Effect.given
      import blog.skeleton.*
      import blog.skeleton.given
      import Exprs.*
      import SkeleExpr.*
  */
  def evalSkeleExpr[F[_]]
    (using err: MonadError[F, Throwable])
    : EvalSkeleExpr[[A] =>> Skele[F, A], SkeleExpr] =
    new EvalSkeleExpr {
    
    import cats.syntax.apply.*
    given Conversion[SkeleExpr, F[SkeleExpr]] = _.pure

    override def variable(name: String) = env ?=> {
      env.get(name) match
        case Some(x) => err.pure(x)
        case None => err.raiseError(Throwable(s"Variable not found: $name"))
    }
    override def evalSkeleLambda(lam: SkeleExpr) = lam match
      case Lambda(_, _) => lam.pure
      case _ => err.raiseError(Throwable(s"$lam is not valid lambda"))
    
    override def integer(i: Int) = Integer(i)
    override def number(n: Double) = Num(n)
    override def string(s: String) = Str(s)
    override def list(xs: List[SkeleExpr]) = Lisp(xs)
    override def lambda(ps: List[SkeleExpr], expr: SkeleExpr) = env ?=> {
      Closure(ps, expr, env).pure
    }
    override def bindings(binds: List[(SkeleExpr, SkeleExpr)], expr: SkeleExpr) = {
      ???
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
            expr.eval(using env)
        }
        case _ => 
          err.raiseError(
            Throwable(s"Application error: found $f")
          )
      end match
    }
  }

end ExprEvaluator
