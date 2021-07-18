package blog.search


sealed trait SearchType
object SearchType {

  case object Tag     extends SearchType
  case object Time    extends SearchType
  case object Contnet extends SearchType
}


trait Search[Query, A] {
  import SearchType._
  def matchBy(method: SearchType)(a: A)(q: Query): Boolean

  def matchTag     = matchBy(Tag)     _
  def matchTime    = matchBy(Time)    _
  def matchContent = matchBy(Contnet) _
}

trait Searchable[Query] {
  def matchBy(method: SearchType)(s: Query): Boolean
}



object Search {
  
  /* In Scala3:
  given Search[A <: Searchable] with
    override def matchBy(method: SearchType)(a: A)(s: String): Boolean = a.matchBy(method)(s)
  */
  class GivenSearchable[Query, A <: Searchable[Query]] extends Search[Query, A] {
    override def matchBy(method: SearchType)(a: A)(s: Query): Boolean = 
      a.matchBy(method)(s)
  }
  implicit def givenSearch[Query, A <: Searchable[Query]]: Search[Query, A] = {
    new GivenSearchable
  }

}
