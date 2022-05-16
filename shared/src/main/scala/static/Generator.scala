package blog.static

import blog.*
import blog.core.*
import blog.page
import scalatags.Text.all.{
  title as titleAttr,
  *
}
import scalatags.Text.tags2.title
import cats.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*



/**
 * The [[Generator]]
*/
import Effect.*
import Effect.given
object StaticBlog {
  
}
// given BlogIndexGenerator[IOErr] = 
//   new BlogIndexGenerator[IOErr](using blog.Configuration.staticBlog) {}


// given BlogIndexGenerator[[A] =>> Injection[IOErr, blog.Configuration, A]] with
//   def config = blog.Configuration.staticBlog
