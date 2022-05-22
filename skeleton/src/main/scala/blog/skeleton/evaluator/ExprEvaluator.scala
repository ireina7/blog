package blog.skeleton.evaluator

import cats.{
  Monad,
  Applicative,
  Traverse,
}
import blog.core.Eval
import blog.skeleton.Exprs.SkeleExpr.*
import blog.skeleton.Exprs.*
import blog.skeleton.Modular

import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.traverse.*
import cats.MonadError
import blog.skeleton.evaluator.EvalSkeleExpr
import blog.core.FileIO



object ExprEvaluator:

  import blog.core.Effect.{*, given}
  import scala.collection.mutable
  type Environment = SkeleExpr.Env
  type Skele[F[_], A] = Injection[F, Environment, A]

  object Environment:
    /** Predefined environment for markdown evaluation
     * encluding:
    */
    def predefForMarkDown: Environment = mutable.Map(
      
    )
  end Environment

  given given_evalSkeleExpr[F[_]](using err: MonadError[F, Throwable])
    : EvalSkeleExpr[[A] =>> Skele[F, A], SkeleExpr] =
    evalSkeleExpr

  /** Implementation for Expression Evaluator (ExprEvaluator,EvalSkeleExpr)
   * evaluate: SkeleExpr => SkeleExpr
   * 
   * - Test imports:
      import blog.core.*
      import blog.core.given
      import Effect.*
      import Effect.given
      import blog.skeleton.*
      import blog.skeleton.given
      import Exprs.*
      import SkeleExpr.*
  */
  // type Inject[Env, A] = Env => Result[A]
  // type F[A] = Inject[Env, A]
  def evalSkeleExpr[F[_]]
    (using err: MonadError[F, Throwable])
    : EvalSkeleExpr[[A] =>> Skele[F, A], SkeleExpr] =
    new EvalSkeleExpr {
    
    import cats.syntax.apply.*
    given Conversion[SkeleExpr, F[SkeleExpr]] = _.pure

    override def variable(name: String) = env ?=> {
      env.get(name) match
        case Some(x) => err.pure(x)
        case None => err.raiseError(blog.Error(s"Variable not found: $name"))
    }
    override def quoted(expr: SkeleExpr) = expr.pure
    // override def evalPattern(pat: SkeleExpr) = pat.pure
    // override def evalSkeleLambda(lam: SkeleExpr) = lam match
    //   case Lambda(_, _) => lam.pure
    //   case _ => err.raiseError(Throwable(s"$lam is not valid lambda"))

    // override def evalSkeleBindings(binds: SkeleExpr) = binds match
    //   case Let(bs, e) => bindings(bs, e)
    //   case _ => err.raiseError(Throwable(s"$binds is not valid binding"))
    
    override def integer(i: Int) = Integer(i)
    override def number(n: Double) = Num(n)
    override def string(s: String) = Str(s)
    override def list(xs: List[SkeleExpr]) = Lisp(xs)
    override def lambda(ps: List[SkeleExpr], expr: SkeleExpr) = env ?=> {
      Closure(ps.map(_.asInstanceOf[Pattern]), expr, env.clone()).pure
    }
    // override def pattern(e: SkeleExpr) = env ?=> e
    override def quote(e: SkeleExpr) = env ?=> e
    override def matching(expr: SkeleExpr, branches: List[(SkeleExpr, SkeleExpr)]) = {
      ???
    }
    override def bindings
      (binds: List[(SkeleExpr, SkeleExpr)], expr: SkeleExpr) = env ?=> {
      
      val innerEnv = env.clone()
      for {
        newEnv <- binds.foldLeft(innerEnv.pure) { 
                    case (fenv, (Var(name), exp)) =>
                      for {
                        env <- fenv
                        res <- exp.eval(using env)
                      } yield {
                        env += (name -> res)
                        env
                      }
                  }
        result <- expr.eval(using newEnv)
      } yield result
    }
    override def application
      (f: SkeleExpr, xs: List[SkeleExpr]): Skele[F, SkeleExpr] = {
      
      f match
        case Closure(ps, expr, env) => {
          val pairs = (ps zip xs)
          val lefts = ps diff pairs.map(_._1)
          pairs.foreach {
            case (Var(name), v) => env += (name -> v)
            case (x, _) => err.raiseError(
              blog.Error(s"""
                |Only application of variable is supported currently: 
                |found $x
              """.stripMargin)
            )
          }
          if !lefts.isEmpty then
            lambda(lefts, expr)(using env)
          else
            expr.eval(using env)
        }
        case _ => 
          err.raiseError(
            blog.Error(s"Application error: found $f")
          )
      end match
    }
    override def set(pat: SkeleExpr, v: SkeleExpr): Skele[F, SkeleExpr] = {
      ???
    }
    override def define
      (name: String, ps: List[SkeleExpr], expr: SkeleExpr): Skele[F, SkeleExpr] = {
      ???
    }
    override def block(states: List[SkeleExpr]): Skele[F, SkeleExpr] = {
      ???
    }
  }
  
end ExprEvaluator






object PreMarkDownExprEvaluator:

  import blog.core.Effect.{*, given}
  import scala.collection.mutable
  /** Reference of values
   * We have to use ref to refer to values stored in the environment.
  */
  case class Ref(var value: SkeleExpr) extends SkeleExpr {
    def update(x: SkeleExpr): this.type = {value = x; this}
  }
  // given Conversion[SkeleExpr, Ref[SkeleExpr]] = Ref.apply
  // given Conversion[Ref[SkeleExpr], SkeleExpr] = _.value
  type Environment = SkeleExpr.Env//mutable.Map[String, Ref[SkeleExpr]]
  type Skele[F[_], A] = Injection[F, Environment, A]

  // SkeleExpr extensions
  case object Primitive extends SkeleExpr
  case class ImplicitParameter(parameter: Pattern) extends Pattern
  case class 
    Macro(name: String, params: List[Pattern], block: List[SkeleExpr])
    extends SkeleExpr

  object Environment:
    def empty: Environment = mutable.Map.empty
    def prim(ps: List[SkeleExpr], expr: SkeleExpr): SkeleExpr = {
      Closure(ps.map(_.asInstanceOf[Pattern]), expr, primitives)
    }
    import blog.skeleton.parser.NaiveParser.given
    val parser = summon[blog.core.Parser[cats.Id, SkeleExpr]]
    given Conversion[String, SkeleExpr] =
      parser.parse(_)
    val prims: Environment = mutable.Map(
      "set"       -> prim(List("\\pattern", "\\self"), "(\\set \\pattern \\self)"),
      "block"     -> prim(List("\\self"), "(\\block \\self)"),
      "space"     -> "\\space",
      "slash"     -> "\\slash",
      "code"      -> prim(List("\\self"), "(\\code \\self)"),
      "pure"      -> prim(List("\\self"), "(\\pure \\self)"),
      "n"         -> "\\n",
      "bold"      -> prim(List("\\self"), "(\\bold \\self)"),
      "italic"    -> prim(List("\\self"), "(\\italic \\self)"),
      "_"         -> prim(List("\\self"), "(\\_ \\self)"),
      "@"         -> prim(List("\\self"), "(\\@ \\self)"),
      "#"         -> prim(List("\\self"), "(\\# \\self)"),
      "##"        -> prim(List("\\self"), "(\\## \\self)"),
      "###"       -> prim(List("\\self"), "(\\### \\self)"),
      "####"      -> prim(List("\\self"), "(\\#### \\self)"),
      "#####"     -> prim(List("\\self"), "(\\##### \\self)"),
      "######"    -> prim(List("\\self"), "(\\###### \\self)"),
      "section"   -> prim(List("\\self"), "(\\section \\self)"),
      "paragraph" -> prim(List("\\self"), "(\\paragraph \\self)"),
      "module"    -> prim(List("\\self"), "(\\module \\self)"),
      "link"      -> prim(List("\\self"), "(\\link \\self)"),
      "image"     -> prim(List("\\self"), "(\\image \\self)"),
      "font"      -> prim(List("\\self"), "(\\font \\self)"),
      "span"      -> prim(List("\\self"), "(\\span \\self)"),
      "line"      -> "\\line",
      "list"      -> prim(List("\\self"), "(\\list \\self)"),
      "video"     -> prim(List("\\self"), "(\\video \\self)"),
      "audio"     -> prim(List("\\self"), "(\\audio \\self)"),
      "source"    -> prim(List("\\self"), "(\\source \\self)"),
      "document"  -> prim(List("\\self"), "(\\document \\self)"),
    )
    val primitives: Environment = mutable.Map(
      "set"       -> Primitive,
      "block"     -> Primitive,
      "space"     -> Primitive,
      "slash"     -> Primitive,
      "code"      -> Primitive,
      "pure"      -> Primitive,
      "n"         -> Primitive,
      "bold"      -> Primitive,
      "italic"    -> Primitive,
      "_"         -> Primitive,
      "@"         -> Primitive,
      "#"         -> Primitive,
      "##"        -> Primitive,
      "###"       -> Primitive,
      "####"      -> Primitive,
      "#####"     -> Primitive,
      "######"    -> Primitive,
      "section"   -> Primitive,
      "paragraph" -> Primitive,
      "module"    -> Primitive,
      "link"      -> Primitive,
      "image"     -> Primitive,
      "font"      -> Primitive,
      "span"      -> Primitive,
      "line"      -> Primitive,
      "list"      -> Primitive,
      "video"     -> Primitive,
      "audio"     -> Primitive,
      "source"    -> Primitive,
      "document"  -> Primitive,
    )
    val arith: Environment = mutable.Map(

    )
    /** Predefined environment for markdown evaluation
     * encluding:
    */
    def predefForMarkDown: Environment = 
      primitives ++ 
      arith ++
      mutable.Map(
        "strong" -> Closure(List(Var("n")), App(Var("bold"), List(Var("n"))), primitives),
        // "box"    -> Closure(List(Var("x")), )
      )
  end Environment

  import blog.core.FileIOString
  import blog.core.Parser
  given given_evalSkeleExpr[F[_]](using 
      err: MonadError[F, Throwable],
      fileIO: FileIO[F, String, String],
      parser: Parser[F, SkeleExpr],
    )
    : (EvalSkeleExpr[[A] =>> Skele[F, A], SkeleExpr] & Modular[[A] =>> Skele[F, A], SkeleExpr]) =
    evalSkeleExpr

  /** Implementation for Expression Evaluator (ExprEvaluator,EvalSkeleExpr)
   * evaluate: SkeleExpr => SkeleExpr
   * 
   * - Test imports:
      import blog.core.*
      import blog.core.given
      import Effect.*
      import Effect.given
      import blog.skeleton.*
      import blog.skeleton.given
      import Exprs.*
      import SkeleExpr.*
  */
  
  def evalSkeleExpr[F[_]]
    (using 
      err: MonadError[F, Throwable], 
      fileIO: FileIO[F, String, String],
      parser: Parser[F, SkeleExpr]
    )
    : EvalSkeleExpr[[A] =>> Skele[F, A], SkeleExpr]
    & Modular[[A] =>> Skele[F, A], SkeleExpr] =
    new EvalSkeleExpr[[A] =>> Skele[F, A], SkeleExpr] 
      with Modular[[A] =>> Skele[F, A], SkeleExpr] {
    
    import cats.syntax.apply.*
    // given Conversion[SkeleExpr, F[SkeleExpr]] = _.pure
    
    override def quoted(expr: SkeleExpr) = expr.pure
    override def variable(name: String) = env ?=> {
      env.get(name) match
        case Some(Primitive) => Var(name).pure
        case Some(Ref(x)) => x.pure
        case Some(x) => x.pure
        case None => 
          // println(s"!!! $name")
          // env.foreach(println)
          // println(env.##)
          err.raiseError(blog.Error(s"Variable not found: $name"))
    }
    // override def evalPattern(pat: SkeleExpr) = pat.pure
    // override def evalSkeleLambda(lam: SkeleExpr) = lam match
    //   case Lambda(ps, exp) => lambda(ps, exp)
    //   case _ => err.raiseError(Throwable(s"$lam is not valid lambda"))

    // override def evalSkeleBindings(binds: SkeleExpr) = binds match
    //   case Let(bs, e) => bindings(bs, e)
    //   case _ => err.raiseError(Throwable(s"$binds is not valid binding"))
    
    override def integer(i: Int) = Integer(i).pure
    override def number(n: Double) = Num(n).pure
    override def string(s: String) = Str(s).pure
    override def list(xs: List[SkeleExpr]) = Lisp(xs).pure
    override def lambda(ps: List[SkeleExpr], expr: SkeleExpr) = env ?=> {
      Closure(ps.map(_.asInstanceOf[Pattern]), expr, env.clone).pure
    }
    // override def pattern(e: SkeleExpr) = env ?=> e.pure
    override def quote(e: SkeleExpr) = env ?=> e.pure
    override def matching(expr: SkeleExpr, branches: List[(SkeleExpr, SkeleExpr)]) = {
      ???
    }
    override def bindings
      (binds: List[(SkeleExpr, SkeleExpr)], expr: SkeleExpr) = env ?=> {
      
      val innerEnv = env.clone()
      for {
        newEnv <- binds.foldLeft(innerEnv.pure) { 
                    case (fenv, (Var(name), exp)) =>
                      for {
                        env <- fenv
                        res <- exp.eval(using env)
                      } yield {
                        env += (name -> res)
                        env
                      }
                  }
        result <- expr.eval(using newEnv)
      } yield result
    }
    override def application
      (f: SkeleExpr, xs: List[SkeleExpr]): Skele[F, SkeleExpr] = env ?=> {
      
      f match
        /**Primitive case, no change! very dirty!*/
        case Var("module") => {
          xs.foldLeft(Pass.pure) { case (acc, x) => 
            for
              pre <- acc
              res <- x.eval(using env)
            yield res
          }
        }
        case Var(name) => App(f, xs).pure
        case Closure(params, expr, environment) => {
          // println(s"$ps, $expr, $env")
          val env = environment.clone()
          var ps = params
          
          val pairs = (ps zip xs)
          val lefts = ps diff pairs.map(_._1)
          val rights = xs diff pairs.map(_._2)
          pairs.foreach {
            case (Var(name), v) => env += (name -> v)
            case (x, _) => err.raiseError(
              blog.Error(s"""
                |Only application of variable is supported currently: 
                |found $x
              """.stripMargin)
            )
          }
          // Handle assignments
          val envSetted = xs.foreach {
            case Set(Quote(Var(s)), v) => 
              // println(s"$s -> $v")
              env += (s -> v)
            case _ => {}
          }
          // println(s"$xs -> $expr")
          // env.foreach(println)
          // println(ps)
          if !lefts.isEmpty then
            lambda(lefts, expr)(using env)
          else if !rights.isEmpty then
            if env.contains("self") 
            then {
              // Set self text
              env("self") = App(Var("span"), env("self") :: rights)
              expr.eval(using env)
            }
            else {
              // Curry it
              // env += ("self" -> App(Var("span"), rights))
              expr match
                case Box(List(f)) => 
                  App(f, rights).eval(using env)
                case _ => 
                  App(expr, rights).eval(using env)
            }
            
          else
            expr.eval(using env)
            // Auto currying
            // if ps.isEmpty || ps.last != Var("self") then
            //   lambda(List(Var("self")), expr)(using env)
            // else 
            //   expr.eval(using env)
            // end if
        }
        case _ => 
          err.raiseError(
            blog.Error(s"Application error: found $f")
          )
      end match
    }
    override def set(pat: SkeleExpr, v: SkeleExpr): Skele[F, SkeleExpr] = env ?=> err.pure {
      pat match
        case Var(s) => 
          if env.contains(s) 
          then env(s) match
            case ref: Ref => ref.update(v)
            case _ => env += (s -> Ref(v))
          else 
            env += (s -> Ref(v))
        case _ => err.raiseError(blog.Error(s"set error: setting $pat"))
      
      // env.foreach(println)
      Set(Quote(pat), v)
    }
    override def define
      (name: String, ps: List[SkeleExpr], expr: SkeleExpr): Skele[F, SkeleExpr] = {
      ???
    }
    override def block(states: List[SkeleExpr]): Skele[F, SkeleExpr] = {
      Pass.pure
    }

    private def arithmetic
      (f: (Double, Double) => Double)(x: SkeleExpr, y: SkeleExpr)
      : Skele[F, SkeleExpr] = {
      
      (x, y) match 
        case (Integer(i), Integer(j)) => Integer(f(i, j).toInt).pure
        case (Num(n), Num(m))         => Num(f(n ,m)).pure
        case (Integer(i), Num(j))     => Num(f(i, j)).pure
        case (Num(n), Integer(m))     => Num(f(n, m)).pure
        case (Box(List(x)), y)        => App(Var("+"), List(x, y)).eval
        case (x, Box(List(y)))        => App(Var("+"), List(x, y)).eval
        case _ => err.raiseError(blog.Error(
          s"Addition error: operating $x and $y"
        ))
    }
    private def `import`(path: String): Skele[F, Unit] = 
      for
        src <- fileIO.readFile(path)
        exp <- parser.parse(src)
        res <- exp.eval
      yield ()

    override def `import`(path: SkeleExpr): Skele[F, Unit] = {
      path match
        case Str(s) => `import`(s)
        case _ => err.raiseError(blog.Error(s"Unsupported import: $path"))
    }


    private def condition
      (branches: List[(SkeleExpr, SkeleExpr)]): Skele[F, SkeleExpr] = {
      
      // branches.foldLeft()
      ???
    }
    private def cases(
      expr: SkeleExpr, 
      branches: List[(SkeleExpr, SkeleExpr)]
    ): Skele[F, SkeleExpr] = {
      
      val notMatch: Skele[F, SkeleExpr] = 
        err.raiseError(
          blog.Error(s"case matching failed: matching: $expr")
        )
      
      branches.foldLeft(notMatch) {
        case (acc, (p, v)) =>
          if p == expr then return v.eval//.flatMap{x => println(x); x.pure}
          else acc
      }
    }

    extension (expr: SkeleExpr)
      override def eval: Skele[F, SkeleExpr] = env ?=>
        // println(s"eval env: ${env.##}, $expr")
        val ans = expr match
        case Pass => Pass.pure
        case Box(List(x)) => 
          val env1 = env.clone()
          x.eval
        case Box(xs) => {
          val env1 = env.clone() // to prevent polluting environment
          for {
            vs <- xs.foldLeft(List.empty[SkeleExpr].pure) {
              (ps, p) =>
                for {
                  xs <- ps
                  pv <- p.eval(using env1)
                } yield pv :: xs // for perfomance
            }.map(_.reverse)
            // param <- ps.traverse(_.eval(using env)) //bad bad bad! never use abstractions you do not familiar with
          } yield Box(vs)
        }
          // xs.traverse(_.eval).map(Box.apply)
        case App(Var("import"), List(Str(path))) =>
          `import`(path) >> Pass.pure
        
        case App(Var("square"), xs) => {
          env.get("square") match
            case None => Box(xs).eval
            case Some(Ref(f)) => for {
              ps <- Box(xs).eval
              vs <- application(f, ps::Nil)
            } yield vs
            case Some(f) => for {
              ps <- Box(xs).eval
              vs <- application(f, ps::Nil)
            } yield vs
        }
        case App(Var("case"), expr :: branches) => 
          val bs = branches.map {
            case App(p, List(v)) => (p, v)
            case other => return 
              err.raiseError(blog.Error(s"case pattern syntax error: $other"))
          }
          // println(s">>> $bs")
          for {
            e <- expr.eval
            v <- cases(e, bs)
          } yield v

        case App(Var("+"), List(a, b)) => for {
          x <- a.eval
          y <- b.eval
          z <- arithmetic(_ + _)(x, y)
        } yield z
        case App(Var("-"), List(a, b)) => for {
          x <- a.eval
          y <- b.eval
          z <- arithmetic(_ - _)(x, y)
        } yield z
        case App(Var("*"), List(a, b)) => for {
          x <- a.eval
          y <- b.eval
          z <- arithmetic(_ * _)(x, y)
        } yield z
        case App(Var("/"), List(a, b)) => for {
          x <- a.eval
          y <- b.eval
          z <- arithmetic(_ / _)(x, y)
        } yield z
        
        case App(f, ps) => {
          val env1 = env.clone() // to prevent polluting environment
          // println(s">>> $ps")
          for {
            func  <- f.eval(using env1)
            param <- ps.foldLeft(List.empty[SkeleExpr].pure) {
              (ps, p) =>
                for {
                  xs <- ps
                  pv <- p.eval(using env1)
                } yield pv :: xs // for perfomance
            }.map(_.reverse)
            // param <- ps.traverse(_.eval(using env)) //bad bad bad! never use abstractions you do not familiar with
            res   <- application(func, param)(using env)
          } yield res
        }
        
        case _ => super.eval(expr)
        // println(s"end` eval $expr")
        ans
      end eval
  }

end PreMarkDownExprEvaluator
