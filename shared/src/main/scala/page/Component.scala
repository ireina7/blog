package blog.page


import scalatags.Text.all.{
    title => titleAttr,
    _
}
import scalatags.Text.tags2.title
import scalatags.Text
import scalatags.Text.TypedTag


object Component {

    def searchBar(attrs: Modifier*): TypedTag[String] = {
        form(cls := "blog-searchBar d-flex")(
            input(
                `class` := "form-control me-2", 
                `type` := "search",
                height := 40,
                placeholder := "order", 
                attr("aria-label") := "Search"
            ),
            button(`class` := "btn btn-outline-success", `type` := "submit", height := 40)("summon")
        )
    }

}
