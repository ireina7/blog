package blog.core


trait Environment[F[_], Env, Query, A] {

  extension (env: Env) 
    def get(query: Query): F[A]
    def add(query: Query, a: A): F[Unit]
    def +=(pair: (Query, A)): F[Unit] = pair match
      case (q, v) => add(q, v)
}


