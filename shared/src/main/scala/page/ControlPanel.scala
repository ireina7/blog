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
        h2("终端·控制台"),
        hr,
        div(cls := "container")(
          div(cls := "row")(
            div(cls := "col")(a(href := "/")("博客")),
            div(cls := "col")(a(href := "#")("功能应用")),
            div(cls := "col")(a(href := "/skeleton")("Skeleton 笔记本")),
          ),
        ),
      )
    )
  }

end ControlPanel
