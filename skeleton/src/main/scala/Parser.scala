package blog.skeleton


import blog.core.Parser
import scala.util.parsing.combinator.RegexParsers
import scala.annotation.targetName
import scala.language.implicitConversions
import blog.skeleton.Exprs.*




import cats.*
import cats.effect.*
import cats.data.*
import cats.syntax.traverse.*

given Parser[Id, SkeleExpr] with {
  def parse(s: String) = Parser.parseSkeleExpr(s).getOrElse(SkeleExpr.Pass)
}
given Parser[IO, SkeleExpr] with {
  def parse(s: String) = IO { Parser.parseSkeleExpr(s).getOrElse(SkeleExpr.Pass) }
}
given Parser[IOErr, SkeleExpr] with {
  def parse(s: String) = 
    Parser.parseSkeleExpr(s) match 
      case Right(exp) => EitherT.right(IO(exp))
      case Left (err) => EitherT.left (IO(err))
}
given Parser[blog.Result, SkeleExpr] with {
  def parse(s: String) = Parser.parseSkeleExpr(s)
}



import blog.core.Effect.{*, given}
given [F[_], Env](using 
    F: Monad[F],
    err: MonadError[F, Throwable],
    rawParser: Parser[blog.Result, SkeleExpr],
  )
  : Parser[[A] =>> Injection[F, Env, A], SkeleExpr] with

  def parse(s: String) = rawParser.parse(s) match
    case Left(er) => err.raiseError(Throwable(er))
    case Right(x) => F.pure(x)
end given




object Parser:
  
  // val identity = "[a-zA-Z0-9~\\[\\]!=-@#$+%^&*_:\";/,|\\_\\.]+".r
  val identity = "[^{}()\\s\\\\]+".r //more general

  import scala.util.parsing.combinator.RegexParsers

  object Combinators extends RegexParsers {
    import SkeleExpr.*


    def variable   = "\\" ~ identity ^^ {
      case _ ~ name => Var.apply(name)
    }
    def real       = "[0-9]+\\.[0-9]*".r ^^ (s => SkeleExpr.number(s.toDouble))
    def integer    = "[0-9]+".r ^^ (s => SkeleExpr.integer(s.toInt))
    def number     = real | integer
    def symbol     = "'" ~ identity  ^^ {
      case _ ~ name => SkeleExpr.variable(name)
    }
    def space      = "\\ " ^^ (_ => Str(" "))
    def quoted     = ("'" ~> "[^']+".r <~ "'") ^^ Str.apply.compose(_.stripMargin)
    def pattern    = symbol | lists
    def text       = "[^{}()\\\\]+".r ^^ SkeleExpr.string
    def lambda     = "(\\" ~> lists ~ expr <~ ")" ^^ {
      case App(x, xs) ~ e => SkeleExpr.lambda(
        (x :: xs).map(o => Quote(o.asInstanceOf[Pattern])), 
        Quote(e)
      )
    }
    def bracketBoxed = ("(\\box " ~> expr.* <~ ")") ~ ("{" ~> expr.* <~ "}").? ^^ {
      case xs ~ None => Box(xs)
      case xs ~ Some(ys) => Box(xs ++ ys)
    }
    def braceBoxed = "{" ~> expr.* <~ "}" ^^ Box.apply
    def boxed = bracketBoxed | braceBoxed

    def comments   = ("(\\doc " ~> expr.* <~ ")") ~ ("{" ~> expr.* <~ "}").? ^^ {
      case _ ~ _ => Box(Str(""):: Nil)
    }
    // def structLambda = ("(\\" ~> lists ~ expr <~ ")") ~ ("{" ~> expr.* <~ "}").?
    def simpleAssignment = ("(\\set " ~> variable ~ expr.+ <~ ")") ^^ {
      case key ~ es => Set(Quote(key), Box(es))
    }
    def structAssign = ("(\\set " ~> variable ~ expr.* <~ ")") ~ ("{" ~> expr.+ <~ "}") ^^ {
      case key ~ xs ~ es => Set(Quote(key), Box(xs ++ es))
    }
    def assignment = structAssign | simpleAssignment
    def simpleDef = ("(\\set " ~> lists ~ expr.+ <~ ")") ^^ {
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
    def structDef = ("(\\set " ~> lists ~ expr.* <~ ")") ~ ("{" ~> expr.+ <~ "}") ^^ {
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
    def definition = simpleDef | structDef
    // def codes      = ("(\\code" ~> ??? <~ ")") ^^ {
    //   case code => App(Var("code"), "[.]")
    // }
    def brackets   = ("(\\bracket " ~> expr.* <~ ")") ~ ("{" ~> expr.* <~ "}").? ^^ {
      case xs ~ None => Box(List(Str("(")) ++ xs ++ List(Str(")")))
      case xs ~ Some(ys) => Box(List(Str("(")) ++ xs ++ ys ++ List(Str(")")))
    }
    def braces   = ("(\\brace " ~> expr.* <~ ")") ~ ("{" ~> expr.* <~ "}").? ^^ {
      case xs ~ None => Box(List(Str("{")) ++ xs ++ List(Str("}")))
      case xs ~ Some(ys) => Box(List(Str("{")) ++ xs ++ ys ++ List(Str("}")))
    }
    def lists      = ("(" ~> expr.*   <~ ")") ^^ {
      case Nil => Pass
      case f::ps => application(f, ps)
    }
    
    def structList = (lists) ~ ("{" ~> expr.* <~ "}").? ^^ {
      case App(f, xs) ~ Some(es) => SkeleExpr.application(f, xs ++ es)
      // case App(f, xs) ~ Some(es) => SkeleExpr.application(f, xs ++ List(App(Var("block"), es)))
      case App(f, xs) ~ _ => SkeleExpr.application(f, xs)
      case Pass ~ _ => Pass
      case _ => ???//Left(blog.Error("unknown parser error"))
    }
    

    def expr: Parser[SkeleExpr] = 
      number     | 
      space      |
      quoted     |
      variable   | 
      text       | 
      boxed      |
      comments   |
      brackets   |
      braces     |
      lambda     |
      assignment |
      definition |
      structList // end


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


