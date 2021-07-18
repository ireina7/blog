package blog.page


import scalatags.Text.all.{
    title => titleAttr,
    _
}
import scalatags.Text.tags2.title
import scalatags.Text
import scalatags.Text.TypedTag


trait Library {
  type HTML = Seq[TypedTag[String]]
  def html: HTML
}

abstract class CSSLibrary (name: String) extends Library
abstract class JSLibrary  (name: String) extends Library
abstract class MiscLibrary(name: String) extends Library



object Component {

  type HTML = String

  val configurations = {
    meta(attr("http-equiv") := "Content-Type", content := "text/html; charset=UTF-8")
  }

  val navigator = {
    div(id := "blog-navigator")
  }

  def mainContent(content: TypedTag[HTML]) = {
    div(id := "blog-content")(content)
  }
  
  val footer: TypedTag[HTML] = {
    div(id := "blog-footer")
  }

  def cssFile(filePath: String) = {
    link(rel := "stylesheet", href := filePath)
  }

  def jsFile(filePath: String) = {
    script(`type` := "text/javascript", src := filePath)
  }

  def text(cls: String)(content: String): TypedTag[HTML] = 
    span(`class` := cls)(content)


  object BootStrap extends CSSLibrary("Bootstrap") {
    def html: HTML = Seq(
      link(
        href := "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
        rel := "stylesheet",
        attr("integrity") := "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
        attr("crossorigin") := "anonymous",
      )
    )
  }

  object Highlight extends JSLibrary("Hightlight") {
    def html: HTML = Seq(
      link(
        rel := "stylesheet",
        href := "/assets/css/highlight/styles/a11y-light.min.css",
      ),
      script(src := "/assets/css/highlight/highlight.min.js"),
    )

    def enable: TypedTag[String] = {
      script("hljs.highlightAll();")
    }
  }
  
  object Dependencies extends MiscLibrary("All libraries") {

    def html: HTML = Seq(
      BootStrap.html,
      Highlight.html,
    ).flatten
  }

}
