package blog.core

trait Show[A] {
  extension (a: A) def show: String
}


trait Debugging[A] {
  extension (a: A) def debug: String
}

