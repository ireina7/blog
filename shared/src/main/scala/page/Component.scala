package blog.page


import blog.*
import scalatags.Text.all.{
    title as titleAttr,
    *
}
import scalatags.Text.tags2.title
import scalatags.Text
import scalatags.Text.TypedTag


trait Library {
  type HTML = Seq[HtmlText]
  def html: HTML
}

abstract class CSSLibrary (name: String) extends Library
abstract class JSLibrary  (name: String) extends Library
abstract class MiscLibrary(name: String) extends Library


case class Item(
  id: Int,
  title: String,
  link: String,
  author: String,
  date: String,
  view: String
)

type Index = List[page.Item]

object Component {


  val configurations = {
    meta(attr("http-equiv") := "Content-Type", content := "text/html; charset=UTF-8")
  }

  val navigator = {
    div(id := "blog-navigator")
  }

  def mainContent(content: HtmlText) = {
    div(id := "blog-content")(content)
  }
  
  val footer: HtmlText = {
    div(id := "blog-footer")
  }

  def cssFile(filePath: String) = {
    link(rel := "stylesheet", href := filePath)
  }

  def jsFile(filePath: String) = {
    script(`type` := "text/javascript", src := filePath)
  }

  def text(cls: String)(content: String): HtmlText = 
    span(`class` := cls)(content)

  def space: HtmlText = raw("&nbsp;")


  object BootStrap extends CSSLibrary("Bootstrap") {
    def html = Seq(
      link(
        href := "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
        rel := "stylesheet",
        attr("integrity") := "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
        attr("crossorigin") := "anonymous",
      )
    )
  }
  
  class HighlightLibrary(conf: blog.Configuration) extends JSLibrary("Hightlight") {
    def html = Seq(
      link(
        rel := "stylesheet",
        href := s"${conf.assetsRoute}/css/highlight/styles/a11y-light.min.css",
      ),
      script(src := s"${conf.assetsRoute}/css/highlight/highlight.min.js"),
    )

    def enable: HtmlText = {
      script("hljs.highlightAll();")
    }
  }

  def Highlight: BlogContext[HighlightLibrary] = conf ?=>
    new HighlightLibrary(conf)
  
  class AllLibraries(conf: blog.Configuration) extends MiscLibrary("All libraries") {

    given blog.Configuration = conf
    def html = Seq(
      BootStrap.html,
      Highlight.html,
    ).flatten
  }

  def Dependencies: BlogContext[AllLibraries] = conf ?=>
    new AllLibraries(conf)

}
