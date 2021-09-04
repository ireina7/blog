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



trait Dsl[F[_]] {
  def filterBy(f: Snapshot => Boolean): F[Snapshot]

  import Snapshot.*
  def filterByTitle(title: Title)        = filterBy(_.title == title)
  def filterByAuthor(author: Author)     = filterBy(_.author == author)
  def filterByDate(date: Date)           = filterBy(_.date == date)
  def filterByDescription(query: String) = filterBy(_.description.contains(query))
}


object Dsl {
  def apply[F[_]](using dsl: Dsl[F]) = dsl

  given dslLocal: Dsl[Id] with
    def filterBy(f: Snapshot => Boolean) = ???

  given dslIO: Dsl[IO] with
    def filterBy(f: Snapshot => Boolean): IO[Snapshot] = {
      ???
    }
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

  given blog.ToHTML[Snapshot] with
    extension (s: Snapshot) def toHTML = ???
}