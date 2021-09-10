package blog


import blog.page.Component
import blog.page.Frame
import scalatags.Text.TypedTag
import scalatags.Text.all.{
  title => titleAttr,
  _
}
import scalatags.Text.tags2.title


object StaticBlog {

  type HtmlText = TypedTag[String]
  given blog.Configuation = blog.Configuation.staticBlog

  def main = {
    ???
  }
  def index(inner: HtmlText = div()): HtmlText = {
    Frame.index(inner)
  }
}