package blog.core


trait Environment[F[_], Env, Query, A] {

  extension (env: Env) 
    def configuration: blog.Configuration
    def query(query: Query): F[A]
    def add(query: Query, a: A): F[Unit]
}


