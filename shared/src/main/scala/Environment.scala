package blog

import cats.*


class BlogEnv[A](val config: blog.Configuration) {

  import scala.collection.mutable.Map
  val memo = Map[String, A]()

  def get(q: String): Option[A] = memo.get(q)
  def put(s: String, a: A): Unit = memo.addOne((s, a))
}


import blog.core.Environment
given [F[_]: Monad, A]: 
  Environment[F, BlogEnv[A], String, A] with {

  extension (env: BlogEnv[A]) 
    inline def configuration: blog.Configuration = env.config
    inline def debugging: Boolean = false
    def query(query: String): F[A] = ???
    def add(query: String, a: A): F[Unit] = ???
}





