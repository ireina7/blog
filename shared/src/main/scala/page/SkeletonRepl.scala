package blog.page


object SkeletonRepl:
  import blog.*
  import Component.*
  import scalatags.Text.all.{
    title as titleAttr,
    *
  }
  import scalatags.Text.tags2.title

  def index: BlogContext[HtmlText] = conf ?=> {
    val elem = form(action := "/compile", method := "post") (
      label(`for` := "src")("Code here."), br,
      textarea(id := "src", name := "src", rows := 20, cols := 80),
      br,br,
      input(`type` := "submit", value := "Submit", `class` := "btn btn-outline-success"),
      // input(`type` := "button", value := "Submit", onclick := "blog.compileSkele()"),
    )
    Frame.index(
      mainContent(
        h2("Skeleton playground"),
        div(id := "skele-compiler-box")(elem)
      )
    )
  }
end SkeletonRepl


