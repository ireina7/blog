package blog.skeleton

import scala.language.implicitConversions
import blog.skeleton.Types.*
import blog.core.Eval




trait MarkDownLanguage[F[_], E]:
  def integer(i: Int)       : F[E]
  def number(value: Double) : F[E]
  def string(s: String)     : F[E]
  def list(xs: List[E])     : F[E]
end MarkDownLanguage

trait LambdaCalculus[F[_], E]:
  def lambda(ps: List[E], expr: E)  : F[E]
  def application(f: E, xs: List[E]): F[E]
end LambdaCalculus

trait BindingDsl[F[_], E]:
  def variable(name: String): F[E]
  def bindings(binds: List[(E, E)], expr: E): F[E]
end BindingDsl


trait Expr[F[_], E] extends 
  LambdaCalculus[F, E], 
  MarkDownLanguage[F, E],
  BindingDsl[F, E]


object Expr:
  given given_expr[F[_], E](using 
    markDown: MarkDownLanguage[F, E],
    lambdaCal: LambdaCalculus[F, E],
    bindDsl: BindingDsl[F, E],
  ): Expr[F, E] = new Expr {
    export markDown.{integer, number, string, list}
    export lambdaCal.{lambda, application}
    export bindDsl.{variable, bindings}
  }
end Expr




/**
 * Optional data matching utilities
*/
object Unpack:

  trait MarkDownLanguage[E]:
    def integer(e: E): Int
    def number (e: E): Double
    def string (a: E): String
    def list   (e: E): List[E]

  trait LambdaCalculus[E]:
    def lambda(e: E)     : (List[E], E)
    def application(e: E): (E, List[E])

  trait BindingDsl[E]:
    def variable(e: E): String
    def bindings(e: E): (List[(E, E)], E)

  trait Expr[E] extends
    LambdaCalculus[E], 
    MarkDownLanguage[E],
    BindingDsl[E]

end Unpack


object Match:

  trait MarkDownLanguage[F[_], E]:
    trait IntegerDsl:
      def apply(i: Int): F[E]
      def unapply(e: E): Option[Int]

    trait NumberDsl:
      def apply(d: Double): F[E]
      def unapply(e: E): Option[Double]

    trait StringDsl:
      def apply(s: String): F[E]
      def unapply(e: E): Option[String]

    // trait ListDsl:
    //   def apply()

end Match









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
    case class Let(bindings: List[(SkeleExpr, SkeleExpr)], expr: SkeleExpr) extends SkeleExpr

    
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
    override def bindings(binds: List[(SkeleExpr, SkeleExpr)], expr: SkeleExpr) =
      Let(binds, expr)
    def closure(ps: List[SkeleExpr], expr: SkeleExpr, env: Env) = Closure(ps, expr, env)
  end SkeleExpr
  
  
end Exprs
