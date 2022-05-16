package blog.page

import blog.{
  BlogContext,
  HtmlText,
}
import scalatags.Text.all.{
  title as titleAttr,
  *
}
import scalatags.Text.tags2.title



object ControlPanel:
  
  def index: BlogContext[HtmlText] = conf ?=> {
    Frame.index(
      div(`class` := "blog-content")(
        ul(
          li("blog"),
          li("application")
        ),
      )
    )
  }

end ControlPanel
