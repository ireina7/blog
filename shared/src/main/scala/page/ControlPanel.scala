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
  import Component.mainContent
  def index: BlogContext[HtmlText] = conf ?=> {
    Frame.index(
      mainContent(
        ul(
          li("blog"),
          li("application")
        ),
      )
    )
  }

end ControlPanel
