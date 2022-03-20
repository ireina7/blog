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



object MarkDownEvaluator:

  import blog.core.Effect.{*, given}
  import scala.collection.mutable
  import blog.HtmlText
  import scalatags.Text.all.{
    title as titleAttr,
    *
  }
  // import scalatags.Text.all.raw
  import scalatags.Text.tags2.title
  import blog.page

  type Skele[F[_], A] = 
    Injection[F, mutable.Map[String, HtmlText], A]

  given given_evalMarkDown[F[_]]
    (using errDsl: MonadError[F, Throwable])
    : EvalSkeleExpr[[A] =>> Skele[F, A], HtmlText] =
    evalMarkDown
  
  def evalMarkDown[F[_]]
    (using errDsl: MonadError[F, Throwable])
    : EvalSkeleExpr[[A] =>> Skele[F, A], HtmlText] = new EvalSkeleExpr {
    
    given Conversion[blog.HtmlText, F[HtmlText]] = _.pure

    override def evalSkeleLambda(lam: SkeleExpr) = 
      errDsl.raiseError(
        Throwable(
          s"Evaluation error: lambda(closure) is not valid markdown value: $lam"
        )
      )

    override def variable(name: String) = env ?=> {
      env.get(name) match
        case Some(x) => x
        case None => errDsl.raiseError(Throwable(s"Variable not found: $name"))
    }
    override def integer(i: Int) = raw(i.toString)
    override def number(n: Double) = raw(n.toString)
    override def string(s: String) = raw(s)
    override def list(xs: List[HtmlText]) = div(xs)
    override def lambda(ps: List[HtmlText], expr: blog.HtmlText) = 
      errDsl.raiseError(
        Throwable(
          s"Evaluation error: lambda(closure) is not valid markdown value."
        )
      )
    override def bindings(binds: List[(HtmlText, HtmlText)], expr: HtmlText) = {
      ???
    }
    override def application
      (f: HtmlText, xs: List[HtmlText]): Skele[F, HtmlText] = {
      val head = f match
        case ff: scalatags.Text.TypedTag[_] => ff.tag
        case _ => return errDsl.raiseError(Throwable(s"Application error."))
      tag(head)(xs)
    }

    extension (expr: SkeleExpr)
      override def eval: Skele[F, HtmlText] = 
        super.eval(expr).map(div(_))
  }

end MarkDownEvaluator



object ExprEvaluator:

  import blog.core.Effect.{*, given}
  type Skele[F[_], A] = Injection[F, SkeleExpr.Env, A]

  given given_evalSkeleExpr[F[_]]
    (using err: MonadError[F, Throwable])
    : EvalSkeleExpr[[A] =>> Skele[F, A], SkeleExpr] =
    evalSkeleExpr

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






object SkeleEvaluator:
  // import cats.catsInstancesForId
  // def d = 0
  // val i = summon[Applicative[cats.Id]]
  import blog.core.Parser

  def predef[F[_]]
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

end SkeleEvaluator
