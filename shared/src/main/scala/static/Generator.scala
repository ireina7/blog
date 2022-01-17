package blog.static

import blog.*
import blog.util.*
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
given Generator[IOErr] with
  def config = blog.Configuation.staticBlog

