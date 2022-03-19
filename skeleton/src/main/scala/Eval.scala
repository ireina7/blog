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



/**
 * You may wonder why not directly use intercection type:
    Eval[F, Input, Output] & MarkDownLanguage[F, Output].

  To explain this, suppose we have a function wnat to output markdown,
  then we want to use some (Eval[F, Input, Output] & MarkDownLanguage[F, Output])
  implicitly. However, the scala compiler shall choose the most specific one
  (i.e., Eval[F, Input, Output] & Expr[F, Output]) which cannot handle lambda value error.
*/
trait EvalMarkDown[F[_], Input, Output] extends 
  Eval[F, Input, Output], 
  MarkDownLanguage[F, Output]

trait EvalExpr[F[_], Input, Output] extends
  Eval[F, Input, Output],
  Expr[F, Output]





trait EvalSkeleToMarkDown[F[_], Value]
  (using
    err: MonadError[F, Throwable],
    bind: BindingDsl[F, Value],
    lambdaCal: LambdaCalculus[F, Value]
  ) 
  extends EvalMarkDown[F, SkeleExpr, Value]:

  extension (expr: SkeleExpr)
    override def eval: F[Value] = {
      expr match
        case Var(name)  => bind.variable(name)
        case Integer(i) => integer(i)
        case Num(n)     => number(n)
        case Str(s)     => string(s)
        case Lisp(xs)   => xs.traverse(_.eval).flatMap(list(_))
        /* 
        * This case is optional.
        * If the target value type does not support lambda calculus,
        * Then this case should produce error. 
        */
        case Lambda(_, _) => err.raiseError(
          Throwable(
            s"Evaluation error: lambda(closure) is not valid markdown value: $expr"
          )
        )
        case App(f, ps) => for {
          function   <- f.eval
          parameters <- ps.traverse(_.eval)
          result     <- lambdaCal.application(function, parameters)
        } yield result
        case _          => err.raiseError(
          Throwable(s"Evaluation error: Unknown expression: $expr")
        )
    }

end EvalSkeleToMarkDown



trait EvalSkele[F[_], Value]
  (using markDown: EvalSkeleToMarkDown[F, Value]) 
  extends EvalExpr[F, SkeleExpr, Value]:

  def evalSkeleLambda(lam: SkeleExpr): F[Value]
  
  extension (expr: SkeleExpr)
    override def eval: F[Value] = {
      expr match
        case Lambda(_, _) => evalSkeleLambda(expr)
        case _ => markDown.eval(expr)
    }

end EvalSkele


object EvalSkele:

  import blog.core.Parser
  
  /**
   * String -> SkeleExpr -> Value
  */
  given [F[_]: Monad, Value](using
      parser: Parser[F, SkeleExpr],
      evalSkele: EvalSkele[F, Value]
    ):
    Eval[F, String, Value] with {

    extension (s: String)
      override def eval: F[Value] = {
        for
          exprs <- parser.parse(s)
          skele <- exprs.eval
        yield skele
      }
  }



end EvalSkele






















import blog.core.Effect.{*, given}
import scala.collection.mutable

type Skele[F[_], A] = Injection[F, SkeleExpr.Env, A]



given [F[_]](using err: MonadError[F, Throwable])
  : LambdaCalculus[F, SkeleExpr] = new LambdaCalculus {

  override def lambda(ps: List[SkeleExpr], expr: SkeleExpr) = {
    ???
  }
  override def application(f: SkeleExpr, xs: List[SkeleExpr]) = {
    ???
  }
}

given [F[_]]
  : BindingDsl[F, SkeleExpr] = new BindingDsl {

  override def variable(name: String) = {
    ???
  }
  override def bindings(binds: List[(SkeleExpr, SkeleExpr)], expr: SkeleExpr) = {
    ???
  }
}

given [F[_]: Monad]
  : MarkDownLanguage[F, SkeleExpr] = new MarkDownLanguage {

  given Conversion[SkeleExpr, F[SkeleExpr]] = _.pure

  override def integer(i: Int) = Integer(i)
  override def number(n: Double) = Num(n)
  override def string(s: String) = Str(s)
  override def list(xs: List[SkeleExpr]) = Lisp(xs)
}


given xxxx[F[_]](using 
    err: MonadError[F, Throwable],
    markDown: MarkDownLanguage[F, SkeleExpr],
    bindDsl: BindingDsl[F, SkeleExpr],
    lamCal: LambdaCalculus[F, SkeleExpr],
  )
  : EvalSkeleToMarkDown[F, SkeleExpr] = new EvalSkeleToMarkDown {
  
  export markDown.{integer, number, string, list}
}





given [F[_]](using 
  err: MonadError[F, Throwable],
  xxx: EvalMarkDown[[A] =>> Skele[F, A], SkeleExpr, SkeleExpr],
):
  EvalSkele[[A] =>> Skele[F, A], SkeleExpr] =
  new EvalSkele {
  
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


class A
class B extends A
given (using A): List[Int] = List(7)

given A = A()
given B = B()








object Eval:
  // import cats.catsInstancesForId
  // def d = 0
  // val i = summon[Applicative[cats.Id]]
  import blog.core.Parser

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
