package blog.skeleton


import blog.util.Parser

/**
 * The data type for interal expression tree
*/
enum SkeleExpr {
  case Var(name: String)
  case Num(value: Double)
  case Str(s: String)
  case Lisp(xs: List[SkeleExpr])

  def variable(name: String) = Var(name)
  def number(value: Double) = Num(value)
  def string(s: String) = Str(s)
  def list(xs: SkeleExpr*) = Lisp(xs.toList)
}



object Parser {
  import cats.*
  import cats.effect.*
  
  given Parser[Id, SkeleExpr] with {
    def parse(s: String) = parseSkeleExpr(s)
  }
  given Parser[IO, SkeleExpr] with {
    def parse(s: String) = IO { parseSkeleExpr(s) }
  }

  def parseSkeleExpr(src: String): SkeleExpr = {
    import scala.util.parsing.combinator.*
    ???
  }

}
