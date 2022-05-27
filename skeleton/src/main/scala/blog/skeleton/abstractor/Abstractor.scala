package blog.skeleton.abstractor


/** Abstracting [[Abstract]] component from evaluated article
 * @tparam F the computation effect
 * @tparam Article type of the abstracting target
 * @tparam Abstract type of the abstracted component
 * 
*/
trait Abstractor[F[_], Article, Abstract] {
  /** Do the extraction work
   * @param article the article
   * @return the abstracted component
  */
  def extract(article: Article): F[Abstract]
}

