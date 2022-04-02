package blog.skeleton

import scala.language.implicitConversions
import blog.skeleton.Types.*
import blog.core.Eval
import blog.skeleton.Exprs.SkeleExpr




trait MarkDownLanguage[F[_], E] {
  def integer(i: Int)       : F[E]
  def number(value: Double) : F[E]
  def string(s: String)     : F[E]
  def list(xs: List[E])     : F[E]
}

trait LambdaCalculus[F[_], E] {
  def lambda(ps: List[E], expr: E)  : F[E]
  def application(f: E, xs: List[E]): F[E]
}

trait PatternMatching[F[_], E] {
  // def pattern(pat: E): F[E]
  def matching(expr: E, branches: List[(E, E)]): F[E]
}

trait BindingDsl[F[_], E] extends PatternMatching[F, E] {
  def variable(name: String): F[E]
  def bindings(binds: List[(E, E)], expr: E): F[E]
}

trait Meta[F[_], E] {
  def quote(e: E): F[E]
}

/** The imperative language dsl (optional)
 * Currently only support assignment.
*/
trait Imperative[F[_], E] {
  /** The [[Set]] operation
   * To reassign variables with [[value]].
   * @param pattern the pattern to be reassigned
   * @param value the value
   * @return The effect
  */
  def set(pattern: E, value: E): F[E]
}

/** The definition language dsl
 * Currently only support variable and function definition.
*/
trait Declaration[F[_], E] {
  def define(name: String, params: List[E], expr: E): F[E]
}

/** Language dsl which support statements */
trait Statement[F[_], E] extends
  Imperative[F, E],
  Declaration[F, E] {
  
  def block(statements: List[E]): F[E]
}

/** The expression */
trait Expr[F[_], E] extends 
  LambdaCalculus[F, E], 
  MarkDownLanguage[F, E],
  BindingDsl[F, E],
  Meta[F, E],
  Statement[F, E]






object Expr:
  given given_expr[F[_], E](using 
    markDown: MarkDownLanguage[F, E],
    lambdaCal: LambdaCalculus[F, E],
    bindDsl: BindingDsl[F, E],
    meta: Meta[F, E],
    states: Statement[F, E],
  ): Expr[F, E] = new Expr {
    export markDown.{integer, number, string, list}
    export lambdaCal.{lambda, application}
    export bindDsl.{variable, matching, bindings}
    export meta.quote
    export states.{set, define, block}
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





// object Trees:

//   trait SkeleTree {
//     def code: String = toString
//   }

//   object SkeleTree extends Statement[cats.Id, SkeleTree]:

//     case object Pass extends SkeleTree

//     case class Set(
//       pattern: SkeleTree, 
//       value: SkeleTree
//     ) extends SkeleTree

//     case class Define(
//       name: SkeleTree, 
//       params: List[SkeleTree], 
//       expr: SkeleTree
//     ) extends SkeleTree

//     case class Block
//       (statements: List[SkeleTree]) extends SkeleTree

//     case class Quote(e: SkeleTree) extends SkeleTree
//     override def set(pattern: SkeleTree, value: SkeleTree) = {
//       Set(pattern, value)
//     }
//     override def define
//       (name: SkeleTree, params: List[SkeleTree], expr: SkeleTree) = {
//       Define(name, params, expr)
//     }
//     override def block(statements: List[SkeleTree]) = {
//       Block(statements)
//     }
//     override def quote(e: SkeleTree) = Quote(e)
//   end SkeleTree

// end Trees




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

    case object Pass extends SkeleExpr
    enum Pattern extends SkeleExpr {
      case Var(name: String)
      case Integer(i: Int)
      case Num(n: Double)
      case Str(s: String)
      case Quote(e: SkeleExpr)
      case App(f: SkeleExpr, xs: List[SkeleExpr])
    }
    export Pattern.*
    case class Lambda(ps: List[Pattern], expr: SkeleExpr) extends SkeleExpr
    case class Closure(ps: List[Pattern], expr: SkeleExpr, env: Env) extends SkeleExpr {
      override def toString = "[Closure]"
    }
    case class Lisp(xs: List[SkeleExpr]) extends SkeleExpr
    case class Let(bindings: List[(Pattern, SkeleExpr)], expr: SkeleExpr) extends SkeleExpr
    case class Set(pattern: Pattern, value: SkeleExpr) extends SkeleExpr
    case class Define(name: String, params: List[Pattern], expr: SkeleExpr) extends SkeleExpr
    case class Block(statements: List[SkeleExpr]) extends SkeleExpr
    case class Box(exprs: List[SkeleExpr]) extends SkeleExpr
    
    
    override def variable(name: String) = Var(name)
    override def integer(i: Int) = Integer(i)
    override def number(value: Double) = Num(value)
    override def string(s: String) = Str(s)
    override def list(xs: List[SkeleExpr]) = Lisp(xs)
    def list(xs: SkeleExpr*) = Lisp(xs.toList)
    override def lambda(ps: List[SkeleExpr], expr: SkeleExpr) = 
      Lambda(ps.map(_.asInstanceOf[Pattern]), expr)
    override def application(f: SkeleExpr, xs: List[SkeleExpr]) = {
      // val cl = f.asInstanceOf[Closure] // Since we are in the Id effect, we have to check...
      App(f, xs)
    }
    override def quote(e: SkeleExpr) = Quote(e)
    def pattern(p: SkeleExpr) = quote(p)
    override def bindings(binds: List[(SkeleExpr, SkeleExpr)], expr: SkeleExpr) =
      Let(binds.map((p, v) => (p.asInstanceOf[Pattern], v)), expr)
    // override def pattern(pat: SkeleExpr) = Pattern(pat)
    override def matching(expr: SkeleExpr, branches: List[(SkeleExpr, SkeleExpr)]) = {
      ???
    }
    override def set(pattern: SkeleExpr, value: SkeleExpr) = {
      pattern match
        case pat: Pattern => Set(pat, value)
        case _ => throw Throwable(s"set must accept patterns, found: $pattern")
    }
    override def define
      (name: String, params: List[SkeleExpr], expr: SkeleExpr) = {
      Define(name, params.map(_.asInstanceOf[Pattern]), expr)
    }
    override def block(statements: List[SkeleExpr]) = {
      Block(statements)
    }
    def closure(ps: List[SkeleExpr], expr: SkeleExpr, env: Env) = 
      Closure(ps.map(_.asInstanceOf[Pattern]), expr, env)
  end SkeleExpr
  
  
end Exprs
