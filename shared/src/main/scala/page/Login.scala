package blog.page

import blog.*
import scalatags.Text.all.{
  title as titleAttr,
  *
}
import scalatags.Text.tags2.title


object Login:
  import Component.*
  
  def index: BlogContext[HtmlText] = conf ?=> {

    val loginForm = form(method := "post", action := "/login/submit")(
      div(`class` := "form-container")(
        label(`for` := "userName")(b("User name")),br,
        input(
          `type` := "text", 
          `class` := "form-control me-2", 
          placeholder := "", 
          name := "userName"
        ),
        br, 
        label(`for` := "password")(b("Password")),br,
        input(
          `type` := "password", 
          `class` := "form-control me-2",
          placeholder := "", 
          name := "password"
        ),
        br, 
        button(`type` := "submit", `class` := "btn btn-outline-success")("Login"),
        br, br,
        a(href := "#")(i("Forgot password?")),
      )
    )
    Frame.index(
      div(`class` := "blog-content")(
        h2("Login account"), br,
        loginForm
      )
    )
  }

end Login
