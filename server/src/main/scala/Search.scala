package blog.search

// import cats.effect.IO


enum SearchType {
  case Tag, Time, Content
}

trait Search[F[_], Query, A] {
  import SearchType.*
  def matchBy(method: SearchType)(a: A)(q: Query): F[Boolean]

  def matchTag     = matchBy(Tag)
  def matchTime    = matchBy(Time)
  def matchContent = matchBy(Content)
}

trait Searchable[F[_], Query] {
  def matchBy(method: SearchType)(s: Query): F[Boolean]
}



object Search {

  type SearchLocal[Query, A] = Search[cats.Id, Query, A]
  
  given [F[_], Query, A <: Searchable[F, Query]]: Search[F, Query, A] with
    override def matchBy(method: SearchType)(a: A)(s: Query): F[Boolean] = 
      a.matchBy(method)(s)

}
