package blog.skeleton


import blog.core.Parser
import scala.util.parsing.combinator.RegexParsers
import scala.annotation.targetName
import scala.language.implicitConversions
import blog.skeleton.Exprs.*




import cats.*
import cats.effect.*

given Parser[Id, SkeleExpr] with {
  def parse(s: String) = Parser.parseSkeleExpr(s).getOrElse(SkeleExpr.Pass)
}
given Parser[IO, SkeleExpr] with {
  def parse(s: String) = IO { Parser.parseSkeleExpr(s).getOrElse(SkeleExpr.Pass) }
}
given Parser[blog.Result, SkeleExpr] with {
  def parse(s: String) = Parser.parseSkeleExpr(s)
}
given Parser[blog.Result, blog.HtmlText] with {
  def parse(s: String) = for {
    skele <- summon[Parser[blog.Result, SkeleExpr]].parse(s)
    htmlT <- Parser.parseSkeleExprToHtml(skele)
  } yield htmlT
}


import blog.core.Effect.{*, given}
given [F[_], Env](using 
  F: Monad[F],
  err: MonadError[F, Throwable],
  rawParser: Parser[blog.Result, SkeleExpr],
): 
  Parser[[A] =>> Injection[F, Env, A], SkeleExpr] with
  def parse(s: String) = rawParser.parse(s) match
    case Left(er) => err.raiseError(Throwable(er))
    case Right(x) => F.pure(x)
end given




object Parser:
  
  
  val identity = "[a-zA-Z0-9~\\[\\]!=-@#$+%^&*_:\";/,|\\_]+".r

  import scala.util.parsing.combinator.RegexParsers

  object Combinators extends RegexParsers {
    import SkeleExpr.*

    def variable   = "\\" ~ identity ^^ {
      case _ ~ name => Var.apply(name)
    }
    def real       = "[0-9]+\\.[0-9]*".r ^^ (s => Num(s.toDouble))
    def integer    = "[0-9]+".r ^^ (s => Integer(s.toInt))
    def number     = real | integer
    def text       = identity ^^ Str.apply
    def lists      = ("(" ~> expr.*   <~ ")") ^^ {
      case Nil => Pass
      case f::ps => application(f, ps)
    }
    // def quote      = ("'" ~> expr) ^^ (x => Lisp(List(Var("'"), x)))
    // def string     = ("{" ~> "[^{}]*".r <~ "}") ^^ Str.apply
    // def structList = (lists) ~ string.? ^^ {
    //   case Lisp(xs) ~ s => Lisp(xs ++ s.map(_ :: Nil).getOrElse(Nil))
    //   // case _ => ???//Left(blog.Error("unknown parser error"))
    // }

    def expr: Parser[SkeleExpr] = 
      number | variable | text | /*quote |*/ /*string | structList |*/ lists

    def read(s: String): blog.Result[SkeleExpr] =
      parse(expr, s) match
        case Success(res, _) => Right(res)
        case Failure(msg, _) => Left(blog.Error(msg))
        case Error(msg, _)   => ??? //error("Error: ", Var(msg))
  }

  def parseSkeleExpr(src: String): blog.Result[SkeleExpr] = {
    import scala.util.parsing.combinator.*
    Combinators.read(src)
  }


  // type SkeleEnvironment = 
  //   scala.collection.mutable.Map[String, SkeleExpr]

  def parseSkeleToHtml(expr: SkeleExpr): blog.Result[blog.HtmlText] = {
    import scalatags.Text.all.{
      title as titleAttr,
      *
    }
    // import scalatags.Text.all.raw
    import scalatags.Text.tags2.title
    import blog.page
    import SkeleExpr.*

    expr match {
      case Integer(i) => Right(raw(i.toString))
      case Num(n) => Right(raw(n.toString))
      case Str(s) => Right(raw(s))
      case Var(s) => ???
      case Closure(ps, expr, env) => ???
      case App(f, xs) => ???
      case Lisp(xs) => ???
    }
  }

  def parseSkeleExprToHtml(sexpr: SkeleExpr): blog.Result[blog.HtmlText] = {
    import scalatags.Text.all.{
      title as titleAttr,
      *
    }
    import scalatags.Text.tags2.title
    import blog.page

    val bodyText = ???

    val htmlText = html(
      head(
        page.Component.configurations,
        title("Ireina's magic"),
      ),
      body(
        bodyText
      )
    )
    Right(htmlText)
  }



end Parser