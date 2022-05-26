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
    def parse(s: String): IOErr[SkeleExpr] = EitherT.apply(IO {
      Parser.parseSkeleExpr(s) 
    })
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
      case Left(er) => err.raiseError(er)
      case Right(x) => F.pure(x)
end given




object Parser:
  
  // val identity = "[a-zA-Z0-9~\\[\\]!=-@#$+%^&*_:\";/,|\\_\\.]+".r
  val identity: Regex = "[^{}()'\\.\"\\[\\]\\s\\\\]+".r //more general

  import scala.util.parsing.combinator.RegexParsers

  object Combinators extends RegexParsers {
    import SkeleExpr.*


    private def variable: Parser[Var] = "\\" ~ identity ^^ {
      case _ ~ name => Var.apply(name)
    }
    private def real       = "\\d+\\.\\d+".r ^^ (s => SkeleExpr.number(s.toDouble))
    private def integer    = "\\d+".r ^^ (s => SkeleExpr.integer(s.toInt))
    private def number     = real | integer
    // private def symbol     = "." ~> identity  ^^ SkeleExpr.string
    
    private def space      = "\\ " ^^ (_ => Str(" "))
    private def quoted     = ("'" ~> "[^']+".r <~ "'") ^^ Str.apply.compose(_.stripMargin)
//    private def pattern    = symbol | lists
    private def text       = "[^{}()\\[\\]\\\\\"]+".r ^^ SkeleExpr.string
    private def simpleLambda     = "(\\lambda" ~> simpleLists ~ expr <~ ")" ^^ {
      case App(x, xs) ~ e => SkeleExpr.lambda(
        (x :: xs).map(o => Quote(o.asInstanceOf[Pattern])), 
        Quote(e)
      )
    }
    private def structLambda = 
      "\\lambda" ~> ("{" ~> expr.* <~ "}") ~ ("{" ~> expr.* <~ "}") ~ ("{" ~> expr.* <~ "}").* ^^ {
      case ps ~ es ~ Nil => Lambda(
        ps.map(o => Quote(o.asInstanceOf[Pattern])),
        Quote(Box(es))
      )
      case ps ~ es ~ xs => App(Lambda(
        ps.map(o => Quote(o.asInstanceOf[Pattern])),
        Quote(Box(es))
      ), xs.flatten)
    }
    // private def haskellLambda = variable ~ "=>"
    private def lambda = simpleLambda | structLambda
    private def dot = "."
    private def bracketBoxed = ("(" ~ "\\box " ~> expr.* <~ ")") ~ ("{" ~> expr.* <~ "}").? ^^ {
      case xs ~ None => Box(xs)
      case xs ~ Some(ys) => Box(xs ++ ys)
    }
    private def singleBracketBoxed = "\\box" ~> ("{" ~> expr.* <~ "}") ^^ {
      xs => Box(xs)
    }
    private def braceBoxed = "\"" ~> expr.* <~ "\"" ^^ Box.apply
    private def boxed = bracketBoxed | singleBracketBoxed | braceBoxed

    private def commonComments   = ("(" ~ "\\doc " ~> expr.* <~ ")") ~ ("{" ~> expr.* <~ "}").? ^^ {
      case _ ~ _ => Box(Str(""):: Nil)
    }
    private def structComments = "\\doc" ~> ("{" ~> expr.* <~ "}") ^^ {
      _ => Box(Str(""):: Nil)
    }
    private def comments = commonComments | structComments
    // def structLambda = ("(\\" ~> lists ~ expr <~ ")") ~ ("{" ~> expr.* <~ "}").?
    private def simpleAssignment = "(" ~ "\\set" ~> variable ~ expr.+ <~ ")" ^^ {
      case key ~ es => Set(Quote(key), Box(es))
    }
    private def structAssign = ("(" ~ "\\set" ~> variable ~ expr.* <~ ")") ~ ("{" ~> expr.+ <~ "}") ^^ {
      case key ~ xs ~ es => Set(Quote(key), Box(xs ++ es))
    }
    private def setAssign = "\\set" ~> ("{" ~> variable ~ expr.+ <~ "}") ^^ {
      case key ~ es => Set(Quote(key), Box(es))
    }
    private def curriedAssign = 
      "\\set" ~> ("{" ~> variable <~ "}") ~ ("{" ~> expr.* <~ "}").+ ^^ {
        case key ~ es => Set(Quote(key), Box(es.flatten))
      }
    private def assignment = 
      structAssign | simpleAssignment | setAssign | curriedAssign
    private def simpleDef = ("(" ~ "\\set" ~> structList ~ expr.+ <~ ")") ^^ {
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
    private def structDef = ("(" ~ "\\set" ~> structList ~ expr.* <~ ")") ~ ("{" ~> expr.+ <~ "}") ^^ {
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
    private def setDef = "\\set" ~> ("{" ~> structList ~ expr.+ <~ "}") ^^ {
      case App(f, ps) ~ es => Set(
        Quote(f),
        Lambda(
          ps.map(x => Quote(x.asInstanceOf[Pattern])), 
          Quote(Box(es))
        )
      )
      case _ => Pass
    }
    private def curriedDef =
      "\\set" ~> ("{" ~> structList <~ "}") ~ ("{" ~> expr.* <~ "}").+ ^^ {
        case App(f, ps) ~ es => Set(
          Quote(f),
          Lambda(
            ps.map(x => Quote(x.asInstanceOf[Pattern])), 
            Quote(Box(es.flatten))
          )
        )
        case _ => Pass
      }
    private def definition = simpleDef | structDef | setDef | curriedDef
    // def codes      = ("(\\code" ~> ??? <~ ")") ^^ {
    //   case code => App(Var("code"), "[.]")
    // }
    private def brackets   = ("(" ~ "\\bracket " ~> expr.* <~ ")") ~ ("{" ~> expr.* <~ "}").? ^^ {
      case xs ~ None => Box(List(Str("(")) ++ xs ++ List(Str(")")))
      case xs ~ Some(ys) => Box(List(Str("(")) ++ xs ++ ys ++ List(Str(")")))
    }
    private def braces   = ("(" ~ "\\brace " ~> expr.* <~ ")") ~ ("{" ~> expr.* <~ "}").? ^^ {
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
    private def simpleLists      = ("(" ~> expr.* <~ ")") ^^ {
      case Nil => Pass
      case f::ps => application(f, ps)
    }
    private def variableList: Parser[SkeleExpr] = 
      variable 
        ~ ("{" ~> expr.* <~"}").+ ^^ {
          case f ~ xs => App(f, xs.flatten)
      }

    private def identityList: Parser[SkeleExpr] = 
      identity
        ~ ("{" ~> expr.* <~"}").* ^^ {
          case s ~ xs => App(Var(s), xs.flatten)
      }

    // private def pureLambda = "lambda" ~> ("{" ~> expr.* <~ "}").+ ^^ {
    //   case es if es.length < 2 => Pass
    //   case es => Lambda(
    //     es.init.flatten.map(o => Quote(o.asInstanceOf[Pattern])),
    //     Quote(Box(es.last))
    //   )
    // }


    // private def structList = lists ~ ("{" ~> expr.* <~ "}").? ^^ {
    //   case App(f, xs) ~ Some(es) => SkeleExpr.application(f, xs ++ es)
    //   case App(f, xs) ~ _ => SkeleExpr.application(f, xs)
    //   case Pass ~ _ => Pass
    //   case _ => ???//Left(blog.Error("unknown parser error"))
    // } | variableList

    private def structList: Parser[SkeleExpr] = 
      simpleLists 
        ~ ("{" ~> expr.* <~ "}").* ^^ {
        case App(f, xs) ~ es => SkeleExpr.application(f, xs ++ es.flatten)
        case Pass ~ _ => Pass
        case other => println(other); ???//Left(blog.Error("unknown parser error"))
    } | variableList

    // private def lists: Parser[SkeleExpr] = 
    //   (structList | variable | number | boxed | squares)
    //     ~ ("." ~> (pureLambda | identityList)).* ^^ {
    //       case x ~ fs => fs.foldLeft(x: SkeleExpr) { (xs, f) =>
    //         f match
    //           case App(f, ps) => App(f, ps ++ List(xs))
    //           case Lambda(ps, e) => App(f, List(xs))
    //       }
    //     }
      

    private def branch: Parser[SkeleExpr] =
      "(" ~> expr ~ expr <~ ")" ^^ {
        case p ~ v => Box(List(p, v))
      }
    private def cases: Parser[SkeleExpr] = 
      ("(" ~> "\\case" ~> expr <~ ")") ~ ("{" ~> branch.+ <~ "}") ^^ {
        case e ~ bs => App(Var("case"), e :: bs)
      }

    private def endDots: Parser[SkeleExpr] = 
      ". " ^^ (_ => Str("."))
    // private def dottyExpr: Parser[SkeleExpr] = 
    //   expr ~ ("." ~> structList).+ ^^ {
    //     case e ~ fs => fs.foldLeft(e) { (es, f) =>
    //       f match
    //         case App(f, ps) => App(f, ps ++ List(es))
    //         case _ => ???
    //     }
    //   } | expr


    private def expr: Parser[SkeleExpr] =
      escapes    |
      endDots    |
      quoted     |
      // symbol     |
      comments   |
      brackets   |
      braces     |
      lambda     |
      assignment |
      definition |
      boxed      |
      structList |
      number     |
      text       | 
      squares    |
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
    res
  }

end Parser









// (\section 1) {
//   This is the first section of my new (\bold blog)!
//   This blog is supported by \Scala.js
// }


