package blog.skeleton

import cats.{
  Monad,
  Applicative,
  Traverse,
}
import blog.core.Eval
import blog.skeleton.Exprs.SkeleExpr.*
import blog.skeleton.Exprs.*
// import blog.skeleton.Trees.*
// import blog.skeleton.Trees.SkeleTree.*

import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.traverse.*
import cats.MonadError



/** The overall trait for expressions that can be evaluated.
 * 
 * You may wonder why not directly use intercection type:
    Eval[F, Input, Output] & MarkDownLanguage[F, Output].

  To explain this, suppose we have a function want to output markdown,
  then we want to use some (Eval[F, Input, Output] & MarkDownLanguage[F, Output])
  implicitly. However, the scala compiler shall choose the most specific one
  (i.e., Eval[F, Input, Output] & Expr[F, Output]) which cannot handle lambda value error.
*/
trait EvalExpr[F[_], Input, Output] extends
  Eval[F, Input, Output],
  Expr[F, Output]

trait EvalStatement[F[_], Input, Output] extends
  Eval[F, Input, Output],
  Statement[F, Output]



trait EvalSkeleExpr[F[_], Value] 
  (using errDsl: MonadError[F, Throwable])
  extends EvalExpr[F, SkeleExpr, Value]:

  /** Function for evaluate [[SkeleExpr]]s.
   * 
   * It's sad that we cannot reuse the [[lambda]] method in trait [[Expr]]
   * because we cannot evaluate parameter list and we do not have this data structure
   * currently in Skele.
   * Maybe we can add weak form term into the data structure someday.
  */
  import cats.implicits.*
  
  def evalQuote(expr: SkeleExpr): F[Value]

  extension (expr: SkeleExpr)
    override def eval: F[Value] = {
      expr match
        case Var(name)  => variable(name)
        case Quote(e)   => evalQuote(e)
        case Integer(i) => integer(i)
        case Num(n)     => number(n)
        case Str(s)     => string(s)
        case Lisp(xs)   => xs.traverse(_.eval).flatMap(list)
        case Let(bs, e) => for {
          binds <- bs.traverse { case (v, exp) =>
            for {
              p <- v.eval
              e <- exp.eval
            } yield (p, e)
          }
          exp <- e.eval
          res <- bindings(binds, exp)
        } yield res
        case Closure(_, _, _) => evalQuote(expr)
        case Lambda(ps, e) => for {
          arg <- ps.traverse(p => p.eval)
          exp <- e.eval
          lam <- lambda(arg, exp)
        } yield lam
        case Set(pat, e) => for {
          p <- pat.eval
          v <- e.eval
          s <- set(p, v)
        } yield s
        case Define(name, ps, e) => for {
          params <- ps.traverse(p => p.eval)
          exp    <- e.eval
          definition <- define(name, params, exp)
        } yield definition
        case Block(states) => states.foldLeft(Pass.eval)(_ *> _.eval)
        case App(f, ps) => for {
          func  <- f.eval
          param <- ps.traverse(_.eval)
          res   <- application(func, param)
        } yield res
        case _ => errDsl.raiseError(
          Throwable(s"Evaluation error: Unknown expression: $expr")
        )
      end match
    }

end EvalSkeleExpr


// trait EvalSkeleTree[F[_], Value]
//   (using 
//     exprEvaluator: EvalSkeleExpr[F, Value],
//     errDsl: MonadError[F, Throwable],
//   ) extends EvalStatement[F, SkeleTree, Value]:

//   import cats.implicits.*
//   import cats.syntax.apply.*

//   def pass: F[Value]
  
//   extension (tree: SkeleTree)
//     override def eval: F[Value] = {
//       tree match
//         case Pass => pass
//         case Set(pat, v) => for {
//           p <- pat.quoted.eval
//           x <- v.eval
//           r <- set(p, x)
//         } yield r
//         case Define(name, params, expr) => for {
//           func  <- name.quoted.eval
//           ps    <- params.traverse(_.quoted.eval)
//           value <- expr.quoted.eval
//           res   <- define(func, ps, value)
//         } yield res
//         case Block(states) => states.foldLeft(Pass.eval)(_ *> _.eval)
//         case App(f, ps) => for {
//           func  <- f.eval
//           param <- ps.traverse(_.eval)
//           res   <- exprEvaluator.application(func, param)
//         } yield res
//         case expr: SkeleExpr => exprEvaluator.eval(expr)
//         case _ => errDsl.raiseError(
//           Throwable(s"Evaluation error: Unknown statement: $tree")
//         )
//     }

// end EvalSkeleTree






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



// object SkeleEvaluator:
//   // import cats.catsInstancesForId
//   // def d = 0
//   // val i = summon[Applicative[cats.Id]]
//   import blog.core.Parser

//   def predef[F[_]]
//     (using parser: Parser[F, SkeleExpr])
//     : SkeleExpr.Env = {
//     import scala.collection.mutable
//     import blog.skeleton.Exprs.*
//     import SkeleExpr.*

//     val env0: SkeleExpr.Env = mutable.Map()
//     val env : SkeleExpr.Env = mutable.Map(
//       "+1" -> Closure(List(Var("x")), Integer(2), env0)
//     )
//     env
//   }

// end SkeleEvaluator
