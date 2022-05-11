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
import blog.skeleton.evaluator


object EvalTests
  extends Properties("Skeleton Evaluator tests"):
  import Effect.{*, given}
  import blog.skeleton.given
  import evaluator.PreMarkDownExprEvaluator
  import evaluator.PreMarkDownExprEvaluator.given

  given exprEnv: evaluator.PreMarkDownExprEvaluator.Environment = 
      evaluator.PreMarkDownExprEvaluator.Environment.predefForMarkDown

  // property("integer") = 
  //   laws.Eval.number[
  //     [A] =>> PreMarkDownExprEvaluator.Environment ?=> IOErr[A], 
  //     SkeleExpr, 
  //     SkeleExpr,
  //   ]
  
end EvalTests



package laws:

  import Exprs.SkeleExpr
  import Exprs.SkeleExpr.*
  import Effect.{*, given}

  object Eval:
    val numberGen: Gen[SkeleExpr] = arbitrary[Int].map(Integer.apply)
    def number[F[_]: blog.core.Runnable, Input, Output]
      (using evaluator: Eval[F, Input, Output]) = 
      // evaluator.eval()
      ???
    end number
  end Eval

end laws
