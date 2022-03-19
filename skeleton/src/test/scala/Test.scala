package blog.skeleton


import blog.core.*
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll


object StringSpecification extends Properties("String"):

  property("startsWith") = 
    forAll { (a: String, b: String) =>
      (a + b).startsWith(a)
    }

  property("concatenate") = 
    forAll { (a: String, b: String) =>
      (a + b).length >= a.length && (a + b).length >= b.length
    }
  
end StringSpecification


object SkeletonTests extends Properties("Skeleton Evaluator"):
  import Exprs.SkeleExpr
  import Effect.{*, given}

  property("test") = ???

end SkeletonTests
