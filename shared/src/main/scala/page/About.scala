package blog.page


import blog.*
import scalatags.Text
import scalatags.Text.all.*
// import cats.data.Reader


object About {
  
  def index: BlogContext[HtmlText] = Frame.index(
    div(`class` := "blog-content")(
      h2("About"),
      p(i(
        """ |This is Ireina, the traveling majo!
            |Nice to meet you!
        """.stripMargin
      )),
      tag("ul")(
        li(i("Github:\t"), a(href := "https://www.github.com/ireina7")("github/ireina7"))
      )
    )
  )

}
