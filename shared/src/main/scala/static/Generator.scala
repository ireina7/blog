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
given BlogIndexGenerator[IOErr] with
  def config = blog.Configuration.staticBlog


given BlogIndexGenerator[[A] =>> Injection[IOErr, blog.Configuration, A]] with
  def config = blog.Configuration.staticBlog
