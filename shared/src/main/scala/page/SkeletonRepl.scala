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
    val nameTag = 
      div(cls := "skele-compiler-input-name md-form amber-textarea active-pink-textarea-2")(
        textarea(
          `class` := "skele-compiler-input-name md-textarea form-control", 
          id := s"name0", 
          name := "name", 
          rows := 1,
          placeholder := "\\res0",
          style := "font-family:monospace; background-color:#fbfaf0;",
          oninput := "this.parentNode.dataset.replicatedValue = this.value",
        ),
        // label(`for` := "name0")("Unit name")
      )
    val submitButton = 
      input(
        cls := "btn btn-outline-info", 
        `type` := "button", 
        value := "\u27f3", 
        onclick := "blog.compileSkele(0)"
      )
    val newButton =
      input(
        `class` := "btn", 
        `type` := "button", 
        value := "\uFF0B", 
        style := "background-color:white;",
        onclick := s"blog.newSkeleCompileUnit()"
      )
    val delButton =
      input(
        `class` := "btn", 
        `type` := "button", 
        value := "\uff0d", 
        style := "background-color:white;",
        onclick := s"blog.newSkeleCompileUnit()"
      )
    val newBlogButton = 
      input(
        `class` := "btn btn-outline-success", 
        `type` := "button", 
        value := "⏏", 
        style := "float: right;",
        onclick := "blog.newBlogSkele(0)",
      )
    // val titleInput =
    //   // div(cls := "skele-compiler-input-name md-form amber-textarea active-pink-textarea-2")(
    //     textarea(
    //       `class` := "md-textarea form-control", 
    //       id := s"title0", 
    //       name := "title", 
    //       rows := 1,
    //       width := 500,
    //       placeholder := s"title here",
    //       style := "font-family:monospace; background-color:white; float:right; display: none",
    //       oninput := "this.parentNode.dataset.replicatedValue = this.value",
    //     )
      // )
        
    val inputArea = 
      div(cls := "md-form pink-textarea active-amber-textarea-2")(
        i(cls := "fas fa-angle-double-right prefix"),
        textarea(
          `class` := "skele-compiler-input md-textarea form-control", 
          id := s"src0", 
          name := "src", 
          rows := 2,
          placeholder := "\\italic { Please code in. }",
          style := "font-family:monospace; margin-bottom: 1rem!important;",
          oninput := "this.parentNode.dataset.replicatedValue = this.value",
        ),
      )
    val outputArea = 
      div(`class` := "skele-compiler-output", id := "output0")
    val elem = form(action := "/compile", method := "post") (
      // label(`for` := "src")("Enjoy."), 
      // input(cls := "form-control me-2", `type` := "text", placeholder := "name"),
      // input(`type` := "submit", value := "Submit", `class` := "btn btn-outline-success"),
      nameTag,
      inputArea,
      submitButton, space,
      delButton,
      newButton,
      newBlogButton,
      // titleInput,
      br, br,
      outputArea,
    )
    val helpButton = 
      a(
        style := "float: right; bottom: 0; font-size: 20;",
        href := ""
      )("help?")
    Frame.index(div(
        h2(style := "display: inline;")(
          span(style := "color:purple; font-weight: bold;")(i("λ ")), i("Skeleton notebook")
        ), helpButton,
        br,
        hr,
        br,
        div(id := "skele-compiler-box")(elem)
      )
    )
  }
end SkeletonRepl


