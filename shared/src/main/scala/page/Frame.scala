package blog.page


import scalatags.Text.all.{
  title => titleAttr,
  _
}
import scalatags.Text.tags2.title
// import scalatags.Text.short._


object Frame {
  import scalatags.Text
  import scalatags.Text.TypedTag
  import Component._


  def index(inner: TypedTag[HTML] = div()): TypedTag[HTML] = {
    html(
      head(
        configurations,
        title("Ireina's magic"),
        cssFile("/assets/css/main.css"),
        Dependencies.html,
      ),
      body(
        navigator,
        br,
        mainContent(inner),
        br,
        hr,
        footer,
        jsFile("/assets/js/main.js"),
        Highlight.enable,
      ),
    )
  }


  def item(
    title: String,
    author: String,
    date: java.util.Date,
    view: TypedTag[HTML]
  ): TypedTag[HTML] = {
    
    div(`class` := "blog-item")(
      a(href := "#", `class` := "blog-item-title")(title),
      p("by ", text("blog-item-author")(author)),
      p(text("blog-item-date")(date.toString)),
      view,
    )
  }

}