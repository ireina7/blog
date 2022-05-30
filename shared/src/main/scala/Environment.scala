package blog

import cats.*
import cats.syntax.applicative.*


class BlogEnv[A](val config: blog.Configuration) {

  import scala.collection.mutable.Map
  val memo = Map[String, A]()

  def get(q: String): Option[A] = memo.get(q)
  def put(s: String, a: A): Unit = memo.addOne((s, a))
}


import blog.core.Environment
given [F[_], A](using M: MonadError[F, Throwable])
  : Environment[F, BlogEnv[A], String, A] with {

  extension (env: BlogEnv[A]) 
    // inline def configuration: blog.Configuration = env.config
    // inline def debugging: Boolean = false
    override def get(query: String): F[A] = 
      env.get(query) match 
        case Some(x) => x.pure
        case None => M.raiseError(blog.Error(s"Not found in environment: $query"))
    override def add(k: String, v: A): F[Unit] = 
      env.put(k ,v).pure
}





