package blog.page


import scalatags.Text.all._
import scalatags.Text

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
    
    def index(results: Text.TypedTag[String] = div()) = Frame.index {
        div(`class` := "blog-content")(
            h2("Filter"),
            br,
            p("Tag:"),
            inputBar,
            br,
            p("Date:"),
            inputBar,
            hr,
            results,
        )
    }

}
