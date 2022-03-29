package blog.skeleton


import blog.core.*
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import blog.skeleton.Exprs.SkeleExpr


object StringSpecification extends Properties("String"):

  property("startsWith") = 
    forAll { (a: String, b: String) =>
      (a + b).startsWith(a)
    }

  property("concatenate") = 
    forAll { (a: String, b: String) =>
      (a + b).length >= a.length && (a + b).length >= b.length
    }

  property("one case") = 1 == 1
  
end StringSpecification




package laws {

  import Exprs.SkeleExpr
  import Exprs.SkeleExpr.*
  import Effect.{*, given}

  object Parser:

    def shouldBeEqual[F[_]: blog.core.Runnable]
      (cases: (String, SkeleExpr)*)
      (using parser: Parser[F, SkeleExpr]) = {
      
      cases.forall { case (src, result) =>
        parser.parse(src).run() == Right(result)
      }
    }

    def `variable`[F[_]: blog.core.Runnable]
      (using parser: Parser[F, SkeleExpr]) = shouldBeEqual(
      
      """\func""" -> Var("func"),
      """\font""" -> Var("font"),
    )

    def `simple function application`[F[_]: blog.core.Runnable]
      (using parser: Parser[F, SkeleExpr]) = shouldBeEqual(

      """(\font small)""" -> 
        App(Var("font"), List(Str("small"))),
      
      """(\title hello, world)""" -> 
        App(Var("title"), List(Str("hello, world"))),
      
      """(\f x (\g y))""" -> 
        App(Var("f"), List(Str("x "), App(Var("g"), List(Str("y")))))
    )

  end Parser

}

object SkeletonTests 
  extends Properties("Skeleton Evaluator"):
  import Effect.{*, given}

  property("`simple function application`[IOErr]") = 
    laws.Parser.`simple function application`[IOErr]

  property("`variable`[IOErr]") = 
    laws.Parser.`variable`[IOErr]
  

end SkeletonTests
