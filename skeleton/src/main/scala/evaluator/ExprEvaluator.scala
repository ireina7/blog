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
  import scala.collection.mutable
  type Environment = SkeleExpr.Env
  type Skele[F[_], A] = Injection[F, Environment, A]

  object Environment:
    /** Predefined environment for markdown evaluation
     * encluding:
    */
    def predefForMarkDown: Environment = mutable.Map(
      
    )
  end Environment

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
        case None => err.raiseError(blog.Error(s"Variable not found: $name"))
    }
    override def quoted(expr: SkeleExpr) = expr.pure
    // override def evalPattern(pat: SkeleExpr) = pat.pure
    // override def evalSkeleLambda(lam: SkeleExpr) = lam match
    //   case Lambda(_, _) => lam.pure
    //   case _ => err.raiseError(Throwable(s"$lam is not valid lambda"))

    // override def evalSkeleBindings(binds: SkeleExpr) = binds match
    //   case Let(bs, e) => bindings(bs, e)
    //   case _ => err.raiseError(Throwable(s"$binds is not valid binding"))
    
    override def integer(i: Int) = Integer(i)
    override def number(n: Double) = Num(n)
    override def string(s: String) = Str(s)
    override def list(xs: List[SkeleExpr]) = Lisp(xs)
    override def lambda(ps: List[SkeleExpr], expr: SkeleExpr) = env ?=> {
      Closure(ps.map(_.asInstanceOf[Pattern]), expr, env.clone()).pure
    }
    // override def pattern(e: SkeleExpr) = env ?=> e
    override def quote(e: SkeleExpr) = env ?=> e
    override def matching(expr: SkeleExpr, branches: List[(SkeleExpr, SkeleExpr)]) = {
      ???
    }
    override def bindings
      (binds: List[(SkeleExpr, SkeleExpr)], expr: SkeleExpr) = env ?=> {
      
      val innerEnv = env.clone()
      for {
        newEnv <- binds.foldLeft(innerEnv.pure) { 
                    case (fenv, (Var(name), exp)) =>
                      for {
                        env <- fenv
                        res <- exp.eval(using env)
                      } yield {
                        env += (name -> res)
                        env
                      }
                  }
        result <- expr.eval(using newEnv)
      } yield result
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
              blog.Error(s"""
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
            blog.Error(s"Application error: found $f")
          )
      end match
    }
    override def set(pat: SkeleExpr, v: SkeleExpr): Skele[F, SkeleExpr] = {
      ???
    }
    override def define
      (name: String, ps: List[SkeleExpr], expr: SkeleExpr): Skele[F, SkeleExpr] = {
      ???
    }
    override def block(states: List[SkeleExpr]): Skele[F, SkeleExpr] = {
      ???
    }
  }

end ExprEvaluator






object PreMarkDownExprEvaluator:

  import blog.core.Effect.{*, given}
  import scala.collection.mutable
  type Environment = SkeleExpr.Env
  type Skele[F[_], A] = Injection[F, Environment, A]

  object Primitive extends SkeleExpr

  object Environment:
    def empty: Environment = mutable.Map.empty
    def primitives: Environment = mutable.Map(
      "set"       -> Primitive,
      "block"     -> Primitive,
      "space"     -> Primitive,
      "slash"     -> Primitive,
      "code"      -> Primitive,
      "pure"      -> Primitive,
      "n"         -> Primitive,
      "bold"      -> Primitive,
      "italic"    -> Primitive,
      "_"         -> Primitive,
      "-"         -> Primitive,
      "*"         -> Primitive,
      "**"        -> Primitive,
      "***"       -> Primitive,
      "****"      -> Primitive,
      "*****"     -> Primitive,
      "******"    -> Primitive,
      "section"   -> Primitive,
      "paragraph" -> Primitive,
      "link"      -> Primitive,
      "image"     -> Primitive,
      "font"      -> Primitive,
      "span"      -> Primitive,
      "line"      -> Primitive,
      "list"      -> Primitive,
      "document"  -> Primitive,
    )
    /** Predefined environment for markdown evaluation
     * encluding:
    */
    def predefForMarkDown: Environment = primitives ++ 
      mutable.Map(
        "strong" -> Closure(List(Var("n")), App(Var("bold"), List(Var("n"))), primitives),
        // "box"    -> Closure(List(Var("x")), )
      )
  end Environment

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
    
    override def quoted(expr: SkeleExpr) = expr.pure
    override def variable(name: String) = env ?=> {
      env.get(name) match
        case Some(Primitive) => Var(name).pure
        case Some(x) => err.pure(x)
        case None => 
          // env.foreach(println)
          err.raiseError(blog.Error(s"Variable not found: $name"))
    }
    // override def evalPattern(pat: SkeleExpr) = pat.pure
    // override def evalSkeleLambda(lam: SkeleExpr) = lam match
    //   case Lambda(ps, exp) => lambda(ps, exp)
    //   case _ => err.raiseError(Throwable(s"$lam is not valid lambda"))

    // override def evalSkeleBindings(binds: SkeleExpr) = binds match
    //   case Let(bs, e) => bindings(bs, e)
    //   case _ => err.raiseError(Throwable(s"$binds is not valid binding"))
    
    override def integer(i: Int) = Integer(i)
    override def number(n: Double) = Num(n)
    override def string(s: String) = Str(s)
    override def list(xs: List[SkeleExpr]) = Lisp(xs)
    override def lambda(ps: List[SkeleExpr], expr: SkeleExpr) = env ?=> {
      Closure(ps.map(_.asInstanceOf[Pattern]), expr, env.clone()).pure
    }
    // override def pattern(e: SkeleExpr) = env ?=> e.pure
    override def quote(e: SkeleExpr) = env ?=> e.pure
    override def matching(expr: SkeleExpr, branches: List[(SkeleExpr, SkeleExpr)]) = {
      ???
    }
    override def bindings
      (binds: List[(SkeleExpr, SkeleExpr)], expr: SkeleExpr) = env ?=> {
      
      val innerEnv = env.clone()
      for {
        newEnv <- binds.foldLeft(innerEnv.pure) { 
                    case (fenv, (Var(name), exp)) =>
                      for {
                        env <- fenv
                        res <- exp.eval(using env)
                      } yield {
                        env += (name -> res)
                        env
                      }
                  }
        result <- expr.eval(using newEnv)
      } yield result
    }
    override def application
      (f: SkeleExpr, xs: List[SkeleExpr]): Skele[F, SkeleExpr] = {
      
      f match
        /**Primitive case, no change! very dirty!*/
        case Var(name) => App(f, xs)
        case Closure(params, expr, environment) => {
          // println(s"$ps, $expr, $env")
          val env = environment.clone()
          var ps = params
          
          val pairs = (ps zip xs)
          val lefts = ps diff pairs.map(_._1)
          val rights = xs diff pairs.map(_._2)
          pairs.foreach {
            case (Var(name), v) => env += (name -> v)
            case (x, _) => err.raiseError(
              blog.Error(s"""
                |Only application of variable is supported currently: 
                |found $x
              """.stripMargin)
            )
          }
          // println(s"$ps, $expr")
          // env.foreach(println)
          // println(ps)
          if !lefts.isEmpty then
            lambda(lefts, expr)(using env)
          else if !rights.isEmpty then
            env += ("self" -> App(Var("span"), rights))
            expr.eval(using env)
          else
            // Auto currying
            if ps.isEmpty || ps.last != Var("self") then
              lambda(List(Var("self")), expr)
            else
              expr.eval(using env)
        }
        case _ => 
          err.raiseError(
            blog.Error(s"Application error: found $f")
          )
      end match
    }
    override def set(pat: SkeleExpr, v: SkeleExpr): Skele[F, SkeleExpr] = env ?=> {
      pat match
        case Var(s) => env += (s -> v)
        case _ => err.raiseError(blog.Error("set error"))
      
      Set(Quote(pat), v)
    }
    override def define
      (name: String, ps: List[SkeleExpr], expr: SkeleExpr): Skele[F, SkeleExpr] = {
      ???
    }
    override def block(states: List[SkeleExpr]): Skele[F, SkeleExpr] = {
      Pass.pure
    }
    extension (expr: SkeleExpr)
      override def eval = expr match
        case Box(xs) => for {
          ys <- xs.traverse(_.eval)
        } yield Box(ys)
        case _ => super.eval(expr)
  }

end PreMarkDownExprEvaluator
