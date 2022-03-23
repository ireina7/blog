package blog.page


import blog.*
import scalatags.Text.all.{
  title as titleAttr,
  *
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
    val theBody = 
      body(
        if conf.blogType == blog.BlogType.Static
        then div(id := "static-content")
        else div(),
        navigator,
        br,
        mainContent(inner),
        br,
        hr,
        footer,
        jsFile(s"$assetPath/js/main.js"),
        Highlight.enable,
      )
    
    html(
      head(
        configurations,
        title("Ireina's magic"),
        cssFile(s"$assetPath/css/main.css"),
        // jsFile(s"$assetPath/js/main.js"),
        Dependencies.html,
      ),
      theBody,
    )
  }


  val item: page.Item => HtmlText = { 
    case page.Item(title, link, author, date, view) => 
      // println(link)
      div(`class` := "blog-item")(
        a(href := link, `class` := "blog-item-title")(title),
        p(i("by "), text("blog-item-author")(author)),
        p(text("blog-item-date")(date)),
        p(view),
      )
  }

}