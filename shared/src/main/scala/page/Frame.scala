package blog.page


import blog.*
import scalatags.Text.all.{
  title => titleAttr,
  _
}
import scalatags.Text.tags2.title
// import scalatags.Text.short._
// import cats.data.Reader
// import cats.data.ReaderT


object Frame {
  import scalatags.Text
  import scalatags.Text.TypedTag
  import Component.*


  def index(inner: HtmlText = div()): BlogContext[HtmlText] = conf ?=> {

    val assetPath = conf.blogType.assetsPath
    
    html(
      head(
        configurations,
        title("Ireina's magic"),
        cssFile(s"$assetPath/css/main.css"),
        Dependencies.html,
      ),
      body(
        navigator,
        br,
        mainContent(inner),
        br,
        hr,
        footer,
        jsFile(s"$assetPath/js/main.js"),
        Highlight.enable,
      ),
    )
  }


  def item(
    title: String,
    author: String,
    date: java.util.Date,
    view: HtmlText
  ): HtmlText = {
    
    div(`class` := "blog-item")(
      a(href := "#", `class` := "blog-item-title")(title),
      p("by ", text("blog-item-author")(author)),
      p(text("blog-item-date")(date.toString)),
      view,
    )
  }

}