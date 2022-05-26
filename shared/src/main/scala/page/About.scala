package blog.page


import blog.*
import scalatags.Text
import scalatags.Text.all.*
// import cats.data.Reader


object About {
  import Component.mainContent
  def index: BlogContext[HtmlText] = Frame.index(
    mainContent(
      h2("About"),
      p(i(
        """ |This is Ireina, the traveling majo!
            |Nice to meet you!
        """.stripMargin
      )),
      tag("ul")(
        li(span(style := "color:green;")("Github:\t"), a(href := "https://www.github.com/ireina7")("github/ireina7"))
      )
    )
  )

}
