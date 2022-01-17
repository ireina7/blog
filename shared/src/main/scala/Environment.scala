package blog

import cats.*



trait Environment[F[_], Env, Query, A] {

  extension (env: Env) 
    def configuration: blog.Configuation
    def query(query: Query): F[A]
    def add(query: Query, a: A): F[Unit]
}



class BlogEnv[A](val config: blog.Configuation) {

  import scala.collection.mutable.Map
  val memo = Map[String, A]()

  def get(q: String): Option[A] = memo.get(q)
  def put(s: String, a: A): Unit = memo.addOne((s, a))
}


given [F[_]: Monad, A]: 
  Environment[F, BlogEnv[A], String, A] with {

  extension (env: BlogEnv[A]) 
    def configuration: blog.Configuation = env.config
    def query(query: String): F[A] = ???
    def add(query: String, a: A): F[Unit] = ???
}





