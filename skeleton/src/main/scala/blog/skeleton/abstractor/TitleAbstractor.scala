package blog.skeleton.abstractor



abstract class TitleAbstractor[F[_], Article, Title] 
  extends Abstractor[F, Article, Title] {
  
  def title(article: Article): F[Title]
  def extract(article: Article): F[Title] =
    title(article)
}


object TitleAbstractor:
  opaque type SkeleTitle = String
  def skeleTitle(s: String): SkeleTitle = s
end TitleAbstractor


class SkeleTitleAbstractor[F[_], SkeleExpr, String]
  extends TitleAbstractor[F, SkeleExpr, String]:

  override def title(expr: SkeleExpr): F[String] = 
    ???
  end title
end SkeleTitleAbstractor


