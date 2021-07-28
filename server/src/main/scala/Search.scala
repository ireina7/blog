package blog.search

import cats.*
import cats.effect.*

/**
 * Try to hit(match) `(a: A)` with `(q: Query)`
 * 
 * @param F the effect of operations
 * @param Query the type of query
 * @param A the compared object
 * @return F(false) is not match
*/
trait Hit[F[_], Query, A]:
  extension (a: A) def hit(q: Query): F[Boolean]


object Hit {

  trait Hitable[F[_], Query]:
    def hit(q: Query): F[Boolean]

  type HitLocal[Query, A] = Hit[Id, Query, A]
  def apply[F[_], Query, A](using ev: Hit[F, Query, A]) = ev
  
  given [F[_], Query, A <: Hitable[F, Query]]: Hit[F, Query, A] with
    extension (a: A) override def hit(q: Query): F[Boolean] = 
      a.hit(q)

}


/**
 * Search in a database
*/
trait Search[F[_], DB, Query]:
  def search[A](db: DB)(q: Query)(using Hit[F, Query, A]): F[Option[A]]




enum Dsl[A] {
  case FilterBy[A](f: Snapshot => Boolean) extends Dsl[A]
}


object Dsl {
  import Snapshot.*

  def filterBy[A](f: Snapshot => Boolean) = FilterBy[A](f)
  def filterByTitle[A](title: Title)        = filterBy[A](_.title == title)
  def filterByAuthor[A](author: Author)     = filterBy[A](_.author == author)
  def filterByDate[A](date: Date)           = filterBy[A](_.date == date)
  def filterByDescription[A](query: String) = filterBy[A](_.description.contains(query))
}



/**
 * The basic snapshot of each article of the blog
*/
case class Snapshot (
  title      : Snapshot.Title,
  author     : Snapshot.Author,
  date       : Snapshot.Date,
  description: String,
  address    : Snapshot.Address, // well, temporarily...
)

object Snapshot {

  opaque type Title   = String
  opaque type Author  = String
  opaque type Address = String
  opaque type Date    = java.util.Date
  
  given Hit.HitLocal[String, Snapshot] with
    extension (a: Snapshot) def hit(q: String): Id[Boolean] = 
      false
  
  given (using local: Hit.HitLocal[String, Snapshot]): Hit[IO, String, Snapshot] with
    extension (a: Snapshot) def hit(q: String): IO[Boolean] = 
      ???
}