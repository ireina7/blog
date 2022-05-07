package blog.skeleton.test

import blog.core.*
import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Test.Parameters
import org.scalacheck.Prop.{
  forAll,
  all,
  propBoolean,
}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import blog.skeleton.Exprs
import blog.skeleton.Exprs.SkeleExpr
import Exprs.SkeleExpr.*


object ParserTests
  extends Properties("Skeleton Parser tests"):
  import Effect.{*, given}
  import blog.skeleton.parser.NaiveParser.given

  override def overrideParameters(p: Parameters) = 
    p.withMinSuccessfulTests(1000)

  
  // Properties
  property("simple function application") = all(
    "IOErr" |: laws.Parser.`simple function application`[IOErr],
  )
  property("variable") = all(
    "IOErr" |: laws.Parser.`variable`[IOErr],
  )

  property("cases") = all(
    "variable: IOErr" |: 
      cases.Parser.`variable`[IOErr],
    "function app: IOErr" |: 
      cases.Parser.`simple function application`[IOErr],
  )

end ParserTests






package cases:

  import Exprs.SkeleExpr
  import Exprs.SkeleExpr.*
  import Effect.{*, given}

  object Parser:
    def `variable`[F[_]: blog.core.Runnable]
      (using parser: Parser[F, SkeleExpr]) = 
      casesOf (
        "\\font" -> Var("font"),
      )

    def `simple function application`[F[_]: blog.core.Runnable]
      (using parser: Parser[F, SkeleExpr]) = 
      casesOf (
        """(\font small)""" -> 
          App(Var("font"), List(Str("small"))),
        
        """(\title hello, world)""" -> 
          App(Var("title"), List(Str("hello, world"))),
        
        /** We expect the white space after `x` 
          * should be included in as normal text.
        */
        """(\f x (\g y))""" -> 
          App(Var("f"), List(Str("x "), App(Var("g"), List(Str("y")))))
      )
  end Parser
end cases


package laws:

  import Exprs.SkeleExpr
  import Exprs.SkeleExpr.*
  import Effect.{*, given}

  object Parser:

    val variableGen: Gen[String] = Gen.identifier
    val skeleVarGen: Gen[String] = Gen.nonEmptyListOf(
      arbitrary[Char].suchThat(c =>
        "[^{}()'\"\\[\\]\\\\\\s]+".r.matches(s"$c")
      )
    ).map(_.mkString)
    // val applyGen: Gen[String] = for {

    // }

    /** Testing variable parsing
     * @tparam F the effect
     * @param parser the summoned parser
    */
    def `variable`[F[_]: blog.core.Runnable]
      (using parser: Parser[F, SkeleExpr]) = 
      
      val alphaVar = forAll(Gen.identifier) { s =>
        shouldBeEqual(s"\\$s" -> Var(s))
      }
      val allVar = forAll(skeleVarGen) { s =>
        shouldBeEqual(s"\\$s" -> Var(s))
      }
      val simpleVar = forAll { (i: Int) => 
        caseOf(s"\\func$i" -> Var(s"func$i")) 
      }

      all(
        "alpha variables" |: alphaVar,
        "all variables"   |: allVar,
        "simple variable" |: simpleVar,
      )
    end `variable`
    

    def `simple function application`[F[_]: blog.core.Runnable]
      (using parser: Parser[F, SkeleExpr]) = 
      
      true
    end `simple function application`

    def `structured function application`[F[_]: blog.core.Runnable]
      (using parser: Parser[F, SkeleExpr]) = 

      true
    end `structured function application`

  end Parser

end laws
