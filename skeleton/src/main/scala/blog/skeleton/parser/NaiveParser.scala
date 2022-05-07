package blog.skeleton.parser

import blog.core.Parser
import blog.skeleton.Exprs.SkeleExpr
import blog.skeleton.Expr
//import parser.Parser.Combinators

import scala.annotation.targetName
import scala.language.implicitConversions
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
//import blog.skeleton.Exprs.*
import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.traverse.*

object NaiveParser:

  import blog.core.Effect.*

  given Parser[Id, SkeleExpr] with {
    def parse(s: String): SkeleExpr =
      Parser.parseSkeleExpr(s).getOrElse(SkeleExpr.Pass)
  }
  given Parser[IO, SkeleExpr] with {
    def parse(s: String): IO[SkeleExpr] =
      IO { Parser.parseSkeleExpr(s).getOrElse(SkeleExpr.Pass) }
  }
  given Parser[IOErr, SkeleExpr] with {
    def parse(s: String): IOErr[SkeleExpr] =
      Parser.parseSkeleExpr(s) match 
        case Right(exp) => EitherT.right(IO(exp))
        case Left (err) => EitherT.left (IO(err))
  }
  given Parser[blog.Result, SkeleExpr] with {
    def parse(s: String): blog.Result[SkeleExpr] =
      Parser.parseSkeleExpr(s)
  }

end NaiveParser



import blog.core.Effect.{*, given}
given [F[_], Env](using 
    F: Monad[F],
    err: MonadError[F, Throwable],
    rawParser: Parser[blog.Result, SkeleExpr],
  )
  : Parser[[A] =>> Injection[F, Env, A], SkeleExpr] with

  def parse(s: String): Injection[F, Env, SkeleExpr] =
    rawParser.parse(s) match
      case Left(er) => err.raiseError(Throwable(er))
      case Right(x) => F.pure(x)
end given




object Parser:
  
  // val identity = "[a-zA-Z0-9~\\[\\]!=-@#$+%^&*_:\";/,|\\_\\.]+".r
  val identity: Regex = "[^{}()'\"\\[\\]\\s\\\\]+".r //more general

  import scala.util.parsing.combinator.RegexParsers

  object Combinators extends RegexParsers {
    import SkeleExpr.*


    private def variable: Parser[Var] = "\\" ~ identity ^^ {
      case _ ~ name => Var.apply(name)
    }
    private def real = "\\d+\\.\\d*".r ^^ (s => SkeleExpr.number(s.toDouble))
    private def integer = "\\d+".r ^^ (s => SkeleExpr.integer(s.toInt))
    private def number = real | integer
//    private def symbol     = "'" ~ identity  ^^ {
//      case _ ~ name => SkeleExpr.variable(name)
//    }
    private def space      = "\\ " ^^ (_ => Str(" "))
    private def quoted     = ("'" ~> "[^']+".r <~ "'") ^^ Str.apply.compose(_.stripMargin)
//    private def pattern    = symbol | lists
    private def text       = "[^{}()\\[\\]\\\\\"]+".r ^^ SkeleExpr.string
    private def lambda     = "(\\" ~> lists ~ expr <~ ")" ^^ {
      case App(x, xs) ~ e => SkeleExpr.lambda(
        (x :: xs).map(o => Quote(o.asInstanceOf[Pattern])), 
        Quote(e)
      )
    }
    private def bracketBoxed = ("(\\box " ~> expr.* <~ ")") ~ ("{" ~> expr.* <~ "}").? ^^ {
      case xs ~ None => Box(xs)
      case xs ~ Some(ys) => Box(xs ++ ys)
    }
    private def singleBracketBoxed = "\\box" ~> ("{" ~> expr.* <~ "}") ^^ {
      xs => Box(xs)
    }
    private def braceBoxed = "\"" ~> expr.* <~ "\"" ^^ Box.apply
    private def boxed = bracketBoxed | singleBracketBoxed | braceBoxed

    private def commonComments   = ("(\\doc " ~> expr.* <~ ")") ~ ("{" ~> expr.* <~ "}").? ^^ {
      case _ ~ _ => Box(Str(""):: Nil)
    }
    private def structComments = "\\doc" ~> ("{" ~> expr.* <~ "}") ^^ {
      _ => Box(Str(""):: Nil)
    }
    private def comments = commonComments | structComments
    // def structLambda = ("(\\" ~> lists ~ expr <~ ")") ~ ("{" ~> expr.* <~ "}").?
    private def simpleAssignment = ("(\\set " ~> variable ~ expr.+ <~ ")") ^^ {
      case key ~ es => Set(Quote(key), Box(es))
    }
    private def structAssign = ("(\\set " ~> variable ~ expr.* <~ ")") ~ ("{" ~> expr.+ <~ "}") ^^ {
      case key ~ xs ~ es => Set(Quote(key), Box(xs ++ es))
    }
    private def setAssign = "\\set" ~> ("{" ~> variable ~ expr.+ <~ "}") ^^ {
      case key ~ es => Set(Quote(key), Box(es))
    }
    private def assignment = structAssign | simpleAssignment | setAssign
    private def simpleDef = ("(\\set " ~> lists ~ expr.+ <~ ")") ^^ {
      case App(f, xs) ~ es => 
        Set(
          Quote(f),
          Lambda(
            xs.map(x => Quote(x.asInstanceOf[Pattern])), 
            Quote(Box(es))
          )
        )
      case _ => Pass // bad!
    }
    private def structDef = ("(\\set " ~> lists ~ expr.* <~ ")") ~ ("{" ~> expr.+ <~ "}") ^^ {
      case App(f, ps) ~ xs ~ ys => 
        Set(
          Quote(f),
          Lambda(
            ps.map(p => Quote(p.asInstanceOf[Pattern])), 
            Quote(Box(xs ++ ys))
          )
        )
      case _ => Pass // bad!
    }
    private def setDef = "\\set" ~> ("{" ~> lists ~ expr.+ <~ "}") ^^ {
      case App(f, ps) ~ es => Set(
        Quote(f),
        Lambda(
          ps.map(x => Quote(x.asInstanceOf[Pattern])), 
          Quote(Box(es))
        )
      )
      case _ => Pass
    }
    private def definition = simpleDef | structDef | setDef
    // def codes      = ("(\\code" ~> ??? <~ ")") ^^ {
    //   case code => App(Var("code"), "[.]")
    // }
    private def brackets   = ("(\\bracket " ~> expr.* <~ ")") ~ ("{" ~> expr.* <~ "}").? ^^ {
      case xs ~ None => Box(List(Str("(")) ++ xs ++ List(Str(")")))
      case xs ~ Some(ys) => Box(List(Str("(")) ++ xs ++ ys ++ List(Str(")")))
    }
    private def braces   = ("(\\brace " ~> expr.* <~ ")") ~ ("{" ~> expr.* <~ "}").? ^^ {
      case xs ~ None => Box(List(Str("{")) ++ xs ++ List(Str("}")))
      case xs ~ Some(ys) => Box(List(Str("{")) ++ xs ++ ys ++ List(Str("}")))
    }
    private def squares = "[" ~> expr.* <~ "]" ^^ {
      xs => App(Var("square"), xs)
    }
    private def slash = "\\\\" ^^ {
      _ => Str("\\")
    }
    private def leftBracket  = "\\(" ^^ { _ => Str("(") }
    private def rightBracket = "\\)" ^^ { _ => Str(")") }
    private def bracketEscaped = leftBracket | rightBracket
    private def leftBrace  = "\\{" ^^ { _ => Str("{") }
    private def rightBrace = "\\}" ^^ { _ => Str("}") }
    private def braceEscaped = leftBrace | rightBrace
    private def singleQuote = "\\'" ^^ { _ => Str("'") }
    private def doubleQuote = "\\\"" ^^ { _ => Str("\"") }
    private def quotesEscaped = singleQuote | doubleQuote
    private def escapes =
      slash 
      | space
      | bracketEscaped 
      | braceEscaped 
      | quotesEscaped
    private def lists      = ("(" ~> expr.*   <~ ")") ^^ {
      case Nil => Pass
      case f::ps => application(f, ps)
    }
    private def variableList = variable ~ ("{" ~> expr.* <~"}") ^^ {
      case f ~ xs => App(f, xs)
    }

    private def structList = lists ~ ("{" ~> expr.* <~ "}").? ^^ {
      case App(f, xs) ~ Some(es) => SkeleExpr.application(f, xs ++ es)
      // case App(f, xs) ~ Some(es) => SkeleExpr.application(f, xs ++ List(App(Var("block"), es)))
      case App(f, xs) ~ _ => SkeleExpr.application(f, xs)
      case Pass ~ _ => Pass
      case _ => ???//Left(blog.Error("unknown parser error"))
    } | variableList


    private def expr: Parser[SkeleExpr] =
      escapes    |
      number     | 
      quoted     |
      squares    |
      text       | 
      boxed      |
      comments   |
      brackets   |
      braces     |
      lambda     |
      assignment |
      definition |
      structList |
      variable   //| 
      // structList // end


    def read(s: String): blog.Result[SkeleExpr] =
      parse(expr, s) match
        case Success(res, _) => Right(res)
        case Failure(msg, _) => Left(blog.Error(msg))
        case Error(msg, _)   => Left(blog.Error(msg))
  }

  def parseSkeleExpr(src: String): blog.Result[SkeleExpr] = {
    import scala.util.parsing.combinator.*
    val res = Combinators.read(src)
    // println(res)
    res
  }

end Parser









// (\section 1) {
//   This is the first section of my new (\bold blog)!
//   This blog is supported by \Scala.js
// }


