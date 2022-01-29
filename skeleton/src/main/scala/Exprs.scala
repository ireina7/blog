package blog.skeleton

import scala.language.implicitConversions
import blog.skeleton.Types.*




trait Expr[F[_], E]:
  def variable(name: String)        : F[E]
  def integer(i: Int)               : F[E]
  def number(value: Double)         : F[E]
  def string(s: String)             : F[E]
  def list(xs: List[E])             : F[E]
  def lambda(ps: List[E], expr: E)  : F[E]
  def application(f: E, xs: List[E]): F[E]
end Expr




object Exprs:

  trait Expression[Expr, Value]:
    extension (expr: Expr) def value: Option[Value]

  // trait Expr[E[_]]:
  //   type FunctionType[A, B]
  //   infix type -> [A, B] = FunctionType[A, B]
  //   def invoke[A, B](lam: E[A -> B]): E[A] -> E[B]
  //   def lambda[A, B](fun: E[A] -> E[B]): E[A -> B]

  // trait MyExpr[Env, A] {
  //   def value: A
  // }

  
  /**
   * The data type for interal expression tree
  */
  // type SkeleExpr = MyExpr[?]
  trait SkeleExpr {
    def code: String = toString
  }
  object SkeleExpr extends Expr[cats.Id, SkeleExpr]:

    type Env = collection.mutable.Map[String, SkeleExpr]

    object Pass extends SkeleExpr
    case class Var(name: String) extends SkeleExpr
    case class Integer(i: Int) extends SkeleExpr
    case class Num(n: Double) extends SkeleExpr
    case class Str(s: String) extends SkeleExpr
    case class Lambda(ps: List[SkeleExpr], expr: SkeleExpr) extends SkeleExpr
    case class Closure(ps: List[SkeleExpr], expr: SkeleExpr, env: Env) extends SkeleExpr
    case class App(f: SkeleExpr, xs: List[SkeleExpr]) extends SkeleExpr
    case class Lisp(xs: List[SkeleExpr]) extends SkeleExpr

    
    override def variable(name: String) = Var(name)
    override def integer(i: Int) = Integer(i)
    override def number(value: Double) = Num(value)
    override def string(s: String) = Str(s)
    override def list(xs: List[SkeleExpr]) = Lisp(xs)
    def list(xs: SkeleExpr*) = Lisp(xs.toList)
    override def lambda(ps: List[SkeleExpr], expr: SkeleExpr) = Lambda(ps, expr)
    override def application(f: SkeleExpr, xs: List[SkeleExpr]) = {
      // val cl = f.asInstanceOf[Closure] // Since we are in the Id effect, we have to check...
      App(f, xs)
    }
    def closure(ps: List[SkeleExpr], expr: SkeleExpr, env: Env) = Closure(ps, expr, env)
  end SkeleExpr
  
  
end Exprs
