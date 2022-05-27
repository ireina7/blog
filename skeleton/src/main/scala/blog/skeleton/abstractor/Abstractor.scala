package blog.skeleton.abstractor


trait Abstractor[F[_], Article, Abstract] {
  def extract(article: Article): F[Abstract]
}

