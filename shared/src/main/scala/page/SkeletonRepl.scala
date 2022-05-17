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
      textarea(id := "src", name := "src", rows := 4, cols := 80),
      br,
      input(`type` := "submit", value := "Submit"),
    )
    Frame.index(
      mainContent(elem)
    )
  }
end SkeletonRepl


