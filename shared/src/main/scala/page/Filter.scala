package blog.page


import blog.*
import scalatags.Text.all.*
import scalatags.Text
// import cats.data.Reader
import scalatags.generic.TypedTag

object Filter {

  def inputBar = {
    input(
      `class` := "form-control me-2", 
      `type` := "search",
      height := 40,
      width := 300,
      placeholder := "order", 
      attr("aria-label") := "Search"
    )
  }
  
  def index(results: HtmlText = div()): BlogContext[HtmlText] = 
    import Component.mainContent
    Frame.index(
      mainContent(
        h2("Filter"),
        br,
        form(cls := "blog-searchBar d-flex")(
          p("Tag:"),
          inputBar,
          br,
          p("Date:"),
          inputBar,
          button(`class` := "btn btn-outline-success", `type` := "submit", height := 40)("summon")
        ),
        hr,
        results,
      )
    )

}
