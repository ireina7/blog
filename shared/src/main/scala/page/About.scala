package blog.page


import blog.*
import scalatags.Text
import scalatags.Text.all._
// import cats.data.Reader


object About {
  
  def index: BlogContext[HtmlText] = Frame.index(
    div(`class` := "blog-content")(
      h2("About"),
      p(
        """ |This is Ireina, the traveling majo!
            |Nice to meet you!
        """.stripMargin
      )
    )
  )

}
