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



/** Evaluator for markdown languages
 * 
 * Example:
    (\** Section 1)
    (\section) {
      Hello, Skeleton!
      This engine is supported by \Scala.js .
      (\image "./k-on.png")
    }
*/
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

  /**
   * The evaluator environment
  */
  type Environment = mutable.Map[String, HtmlText]
  object Environment:
    val empty: Environment = mutable.Map.empty
    /**
     * The default environment.
     * Necessary predefined elements are included:
        - bold
        - italic
        - section
        - image
        - link
    */
    val predef: Environment = mutable.Map(
      "set"       -> tag("key"),
      "n"         -> br,
      "bold"      -> b,
      "italic"    -> i,
      "*"         -> h1,
      "**"        -> h2,
      "***"       -> h3,
      "****"      -> h4,
      "*****"     -> h5,
      "******"    -> h6,
      "section"   -> tag("section"),
      "paragraph" -> p,
      "link"      -> a,
      "image"     -> img,
      "font"      -> span,
      "document"  -> div(marginLeft := 50, marginRight := 50, fontSize := 20),
      // "section" -> section,
    )
  end Environment

  type Skele[F[_], A] = 
    Injection[F, Environment, A]

  // For test
  type Effect[A] = Skele[IOErr, A]

  given given_evalMarkDown[F[_]](using errDsl: MonadError[F, Throwable])
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
    override def evalSkeleBindings(binds: SkeleExpr) = binds match
      // case Let(bs, e) => bindings(bs, e)
      case _ => errDsl.raiseError(Throwable(s"$binds is not valid binding"))

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
    override def bindings
      (binds: List[(HtmlText, HtmlText)], expr: HtmlText) = {
      ???
    }
    override def pattern(e: HtmlText) = e.pure
    override def matching(expr: HtmlText, branches: List[(HtmlText, HtmlText)]) = {
      ???
    }

    def applyKey(xs: List[HtmlText]): F[HtmlText] = {
      val ans = tag("key")
      // if(xs.length <= 1) {
      //   return errDsl.raiseError(Throwable(
      //     s"Set key error: $xs"
      //   ))
      // }
      // (xs.head, xs(1)) match
      //   case (Var(name), RawFrag(value)) => ans(attr(name) := value)
      for(x <- xs) {
        x match
          case RawFrag(ss) => 
            val (k, rv) = ss.span(_ != ' ')
            val v = rv.trim
            return ans(attr(k) := v)
      }
      ans
    }
    override def application
      (f: HtmlText, xs: List[HtmlText]): Skele[F, HtmlText] = {
      val (head, attrs) = f match
        case ff: scalatags.Text.TypedTag[_] => (ff.tag, ff.modifiers)
        case _ => return errDsl.raiseError(Throwable(s"Application error."))
      // println(s"$f, $head")
      if(head == "key") {
        return applyKey(xs)
      }
      val ps = xs.filter { x =>
        x match
          case x: scalatags.Text.TypedTag[_] => x.tag != "key"
          case _ => true
      }
      val keys = xs.collect {
        case x: scalatags.Text.TypedTag[_] 
          if x.tag == "key" => x.modifiers
      }
      // println(keys)
      tag(head)(attrs, keys)(ps)
    }

    // extension (expr: SkeleExpr)
    //   override def eval: Skele[F, HtmlText] = 
    //     super.eval(expr).map(div(_))
  }

end MarkDownEvaluator
