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
          placeholder := "definiton",
          style := "font-family:monospace; background-color:#fbfaf0;",
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
        
    val inputArea = 
      div(cls := "md-form mb-4 pink-textarea active-amber-textarea-2")(
        i(cls := "fas fa-angle-double-right prefix"),
        textarea(
          `class` := "skele-compiler-input md-textarea form-control", 
          id := s"src0", 
          name := "src", 
          rows := 4,
          placeholder := "\\italic { Please code in. }",
          style := "font-family:monospace;",
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
      submitButton,
      newButton,
      br, br,
      outputArea,
    )
    val helpButton = 
      a(
        style := "float: right; bottom: 0; font-size: 20;",
        href := ""
      )("help?")
    Frame.index(div(
        h1(style := "display: inline;")(i("Skeleton notebook")), helpButton,
        br,
        hr,
        br,
        div(id := "skele-compiler-box")(elem)
      )
    )
  }
end SkeletonRepl


