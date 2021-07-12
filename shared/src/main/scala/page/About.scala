package blog.page


import scalatags.Text.all._


object About {
    
    def index = Frame.index(
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
