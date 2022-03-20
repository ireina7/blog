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

  To explain this, suppose we have a function want to output markdown,
  then we want to use some (Eval[F, Input, Output] & MarkDownLanguage[F, Output])
  implicitly. However, the scala compiler shall choose the most specific one
  (i.e., Eval[F, Input, Output] & Expr[F, Output]) which cannot handle lambda value error.
*/
// trait EvalToMarkDown[F[_], Input, Output] extends 
//   Eval[F, Input, Output], 
//   MarkDownLanguage[F, Output]

trait EvalExpr[F[_], Input, Output] extends
  Eval[F, Input, Output],
  Expr[F, Output]



object MarkDownEvaluator:

  trait EvalSkeleExpr[F[_], Value]
    (using errDsl: MonadError[F, Throwable])
    extends EvalExpr[F, SkeleExpr, Value]:

    val evalMarkdown: PartialFunction[SkeleExpr, F[Value]] = {
      
      case Var(name)  => variable(name)
      case Integer(i) => integer(i)
      case Num(n)     => number(n)
      case Str(s)     => string(s)
      case Lisp(xs)   => xs.traverse(_.eval).flatMap(list)
      case App(f, ps) => for {
        function   <- f.eval
        parameters <- ps.traverse(_.eval)
        result     <- application(function, parameters)
      } yield result
    }

    extension (expr: SkeleExpr)
      override def eval: F[Value] = evalMarkdown.orElse {
        case Lambda(_, _) => errDsl.raiseError(
          Throwable(
            s"Evaluation error: lambda(closure) is not valid markdown value: $expr"
          )
        )
        case _ => errDsl.raiseError(
          Throwable(s"Evaluation error: Unknown expression: $expr")
        )
      }(expr)

  end EvalSkeleExpr

end MarkDownEvaluator



object ExprEvaluator:

  trait EvalSkeleExpr[F[_], Value]
    (using errDsl: MonadError[F, Throwable])
    extends EvalExpr[F, SkeleExpr, Value]:

    def evalSkeleLambda(lam: SkeleExpr): F[Value]
    
    extension (expr: SkeleExpr)
      override def eval: F[Value] = {
        expr match
          case Var(name)  => variable(name)
          case Integer(i) => integer(i)
          case Num(n)     => number(n)
          case Str(s)     => string(s)
          case Lisp(xs)   => xs.traverse(_.eval).flatMap(list)
          case App(f, ps) => for {
            function   <- f.eval
            parameters <- ps.traverse(_.eval)
            result     <- application(function, parameters)
          } yield result
          case Lambda(_, _) => evalSkeleLambda(expr)
          case _ => errDsl.raiseError(
            Throwable(s"Evaluation error: Unknown expression: $expr")
          )
        end match
      }
  end EvalSkeleExpr

end ExprEvaluator





import blog.core.Parser

/**
 * String -> SkeleExpr -> Value
*/
given [F[_]: Monad, Value](using
    parser: Parser[F, SkeleExpr],
    evalIt: Eval[F, SkeleExpr, Value]
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












import blog.core.Effect.{*, given}

/** Skele Effect
 * 
*/
type Skele[F[_], A] = Injection[F, SkeleExpr.Env, A]


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
    import blog.skeleton.EvalSkele.given
*/
given [F[_]]
  (using err: MonadError[F, Throwable])
  : ExprEvaluator.EvalSkeleExpr[[A] =>> Skele[F, A], SkeleExpr] =
  new ExprEvaluator.EvalSkeleExpr {
  
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
