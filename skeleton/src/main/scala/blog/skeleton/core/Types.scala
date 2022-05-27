package blog.skeleton

object Types:

  /** Type encodings
   * Initial encoding is enough for skeleton
   */
  trait Type {
    def toString: String
  }

  enum PrimType extends Type {
    case Unit, Integer, Real, String, Bool
    case Func[A, B](a: A, b: B)
  }

  case class HigherKind(hkt: List[StaticType] => StaticType) {
    override def toString = s"[Higher kind]"
  }

  enum StaticType extends Type {
    case Prim(t: PrimType)
    case ListOf(f: HigherKind, ts: List[StaticType])
    case Apply(f: HigherKind, ts: List[StaticType])

    override def toString = this match
      case Prim(t) => t.toString
      case ListOf(f, ts) => s"${f.toString}[${ts.map(_.toString).mkString(", ")}]"
      case Apply(_, _) => s"[Apply]"
  }

  export StaticType.*

end Types
