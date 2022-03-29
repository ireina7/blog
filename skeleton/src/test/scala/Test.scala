package blog.skeleton.test


import blog.core.*
import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Test
import org.scalacheck.Prop.{
  forAll,
  all,
  propBoolean,
}
import org.scalacheck.Gen
import blog.skeleton.Exprs
import blog.skeleton.Exprs.SkeleExpr





// object StringSpecification extends Properties("String"):

//   property("startsWith") = 
//     forAll { (a: String, b: String) =>
//       (a + b).startsWith(a)
//     }

//   property("concatenate") = 
//     forAll { (a: String, b: String) =>
//       (a + b).length >= a.length && (a + b).length >= b.length
//     }

//   property("one case") = 1 == 1
  
// end StringSpecification


// class Cases(name: String, props: Prop*) extends Properties(name):

//   // include(props)
//   override def overrideParameters(p: Test.Parameters) = 
//     p.withMinSuccessfulTests(1)
  
// end Cases


def shouldBeEqual[F[_]: blog.core.Runnable]
  (cases: (String, SkeleExpr)*)
  (using parser: Parser[F, SkeleExpr]) = {

  cases.forall { case (src, result) =>
    // println(s"$src -> ${parser.parse(src).run()}")
    parser.parse(src).run() == Right(result)
  }//.set(minTestsOk = 1)
}
def casesOf[F[_]: blog.core.Runnable]
  (cases: (String, SkeleExpr)*)
  (using parser: Parser[F, SkeleExpr]) = {
  shouldBeEqual(cases*)
}
def caseOf[F[_]: blog.core.Runnable]
  (pair: (String, SkeleExpr))
  (using parser: Parser[F, SkeleExpr]) = {
  shouldBeEqual(pair)
}
def shouldNotBeEqual[F[_]: blog.core.Runnable]
  (cases: (String, SkeleExpr)*)
  (using parser: Parser[F, SkeleExpr]) = {
  
  cases.forall { case (src, result) =>
    parser.parse(src).run() != Right(result)
  }
}
def shouldFail[F[_]: blog.core.Runnable]
  (cases: String*)
  (using parser: Parser[F, SkeleExpr]) = {
  
  cases.forall { src =>
    parser.parse(src).run() match
      case Left(_) => true
      case _ => false
  }
}





object SkeletonTests 
  extends Properties("Skeleton tests"):
  import Effect.{*, given}
  import blog.skeleton.given

end SkeletonTests
