package blog
// import scala.scalajs.js.annotation.JSExportTopLevel

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.document
import js.annotation.{
  JSExportTopLevel,
  JSExport,
}
import scalatags.JsDom.all._
import org.scalajs.dom.raw.HTMLTextAreaElement



@JSExportTopLevel("blog")
object Main:

  def main(args: Array[String]): Unit = {
    println("Hello world!")
    document.addEventListener("DOMContentLoaded", (e: dom.Event) => {
      setup()
    })
  }

  /**
   * It is sad that we cannot reuse [[blog.BlogType]] here.
  */
  val assetsPath = //Configuration.staticBlog.blogType.assetsPath
    if isStatic then "../../assets" else "/assets"

  def setup(): Unit = {
    
    document.body.appendChild(sideBar)
    document.getElementById("blog-navigator").appendChild(navBar)
    document.getElementById("blog-footer"   ).appendChild(footer)
    // ScalaJSExample.draw(canvas)
    // ScalaJSExample.clock(canvas)
  }

  def isStatic: Boolean = 
    document.getElementById("static-content") != null

  def setupStaticBlogIfNecessary(): Unit = {

    val staticBlog: dom.Element | Null = document.getElementById("static-content")
    if staticBlog != null then
      println("Static blog has been set up.")
      staticBlog.appendChild(navBar)
      staticBlog.appendChild(footer)
  }


  val footer: dom.Element = {

    tag("footer")(`class` := "text-lg-start", marginLeft := 30)(
      div(`class` := "p-1")(
        p(
          i("Powered by "), 
          span(color := "red")(i(b("Scala."))), span(color := "orange")(i(b("js")))
        )
      ),
    )
  }.render

  def spinner: dom.Element = {

    div(`class` := "spinner-border", role := "status")(
      span(`class` := "visually-hidden")("Loading...")
    )
  }.render

  val searchBar: dom.Element = {

    form(cls := "blog-searchBar d-flex")(
      input(
        `class` := "form-control me-2", 
        `type` := "search",
        height := 40,
        placeholder := "order", 
        attr("aria-label") := "Search"
      ),
      raw("&nbsp"), raw("&nbsp"), raw("&nbsp"),
      button(`class` := "btn btn-outline-success", `type` := "submit", height := 40)("summon")
    )
  }.render

  val sideBar: dom.Element = {

    div(id := "mySidenav", `class` := "sidenav")(
      a(href := "/")("主页"),
      a(href := "/login")("登录"),
      a(href := "/about")("关于"),
      a(href := "/filter")("过滤"),
      // a(href := "/skeleton")("Skeleton"),
      a(href := "#")("范畴"),
      a(href := "#")("结构"),
      a(href := "#", onclick := "blog.changeMainContentMode()")("模式"),
      a(href := "/logout")("登出"),
    )
  }.render

  @JSExport
  val sideBarWidth = "500px"
  @JSExport
  var sideBarShow = false
  
  @JSExport
  def openSideBar(): Unit = {
    document.getElementById("mySidenav").asInstanceOf[html.Element].style.width = sideBarWidth
    sideBarShow = true
  }
  @JSExport
  def closeSideBar(): Unit = {
    document.getElementById("mySidenav").asInstanceOf[html.Element].style.width = "0";
    sideBarShow = false
  }
  
  val navBar: dom.Element = {
      
    div(
      backgroundImage := s"url($assetsPath/img/yuii.png)", 
      backgroundPosition := "right bottom",
      backgroundSize := "cover",
    )(
      tag("nav")(`class` := "blog-navigator nav sticky-top")(
        div(marginTop := 20)(
          a(
            `class` := "navbar-brand",
            href := "#",
            onclick := { () => if (!sideBarShow) openSideBar() else closeSideBar() }
          )(
            img(`class` := "spinner", src := s"$assetsPath/img/lambda-icon-18.jpg", width := 70),
          ),
          span(`class` := "navbar-text", fontSize := 30)(i(b(span(color := "purple")("Ireina's "), "magic"))),
        ),
        i(fontSize := 20)(b("TM"))
        // searchBar,
      ),
      searchBar,
      pre(
        marginLeft := 60,
        fontSize := 18,
      )(
        """
        |Y = λf. (λx. f (x x)) (λx. f (x x))
        |
        |trait Monad[M[_]] {
        |  def pure[A](a: A): M[A]
        |  extension [A](ma: M[A]) def flatMap[B](f: A => M[B]): M[B]
        |}
        """.stripMargin
      )
    )
  }.render

  /** AJAX function for skele dynamic compiler
   * skele-compiler-box {
   *   skele-compiler-input
   *   skele-compiler-output
   *   ...skele-compiler-units
   * }
  */
  @JSExport
  var srcId: Int = 1
  @JSExport
  def newSkeleCompileUnit(): Unit = {
    document.getElementById("skele-compiler-box")
      .appendChild {
        val inputArea = div(cls := "md-form pink-textarea active-amber-textarea-2")(
          i(cls := "fas fa-angle-double-right prefix"),
          textarea(
            `class` := "skele-compiler-input md-textarea form-control", 
            id := s"src$srcId", 
            name := "src", 
            rows := 2,
            style := "font-family:monospace; margin-bottom: 1rem!important;",
            oninput := "this.parentNode.dataset.replicatedValue = this.value",
          ),
          // label(`for` := "Code here"),
        )
        val outputArea = 
          div(`class` := "skele-compiler-output", id := s"output$srcId")
        val submitButton =
          input(`class` := "btn btn-outline-info", `type` := "button", value := "\u27f3", onclick := s"blog.compileSkele(${srcId})")
        val newButton =
          input(`class` := "btn", `type` := "button", value := "\uFF0B", onclick := s"blog.newSkeleCompileUnit()", style := "background-color:white;")
        val delButton =
          input(
            `class` := "btn", 
            `type` := "button", 
            value := "\uff0d", 
            style := "background-color:white;",
            onclick := s"blog.newSkeleCompileUnit()"
          )
        val titleInput =
          // div(cls := "skele-compiler-input-name md-form amber-textarea active-pink-textarea-2")(
          textarea(
            `class` := "md-textarea form-control", 
            id := s"title$srcId", 
            name := "title", 
            rows := 1,
            // width := 350,
            placeholder := s"title here",
            style := "width: 500px; font-family:monospace; background-color:white; float:right; display: none",
            oninput := "this.parentNode.dataset.replicatedValue = this.value",
          )
          // )
        val newBlogButton = 
          input(
            `class` := "btn btn-outline-success", 
            `type` := "button", 
            value := "⏏", 
            style := "float: right;",
            onclick := s"blog.newBlogSkele(${srcId})",
          )
        val variableTag =
          div(cls := "skele-compiler-input-name md-form amber-textarea active-pink-textarea-2")(
            textarea(
              `class` := "skele-compiler-input-name md-textarea form-control", 
              id := s"name$srcId", 
              name := "name", 
              rows := 1,
              placeholder := s"\\res$srcId",
              style := "font-family:monospace; background-color:#fbfaf0;",
              oninput := "this.parentNode.dataset.replicatedValue = this.value",
            ),
            // label(`for` := "name0")("Unit name")
          )

        div(
          br,
          variableTag,
          inputArea,
          submitButton,
          // delButton,
          newButton,
          newBlogButton,
          titleInput,
          br, br,
          outputArea,
        ).render
      }
      activeTextareaAutoExpand()
      srcId += 1
  }
  @JSExport
  def compileSkele(curId: Int): Unit = {
    val httpReq = new dom.XMLHttpRequest()
    // println(c·urId)
    httpReq.onreadystatechange = event =>
      if (httpReq.readyState == 4 && httpReq.status == 200)
      then {
        val outputArea = document.getElementById(s"output$curId")
        outputArea.innerHTML = httpReq.responseText
      }
    httpReq.open("POST", "/compile", true)
    // httpReq.setRequestHeader("Content-type", "application/x-www-form-urlencoded")
    val content = document
      .getElementById(s"src$curId")
      .asInstanceOf[HTMLTextAreaElement]
      .value
    val nameTag = document
      .getElementById(s"name$curId")
      .asInstanceOf[HTMLTextAreaElement]
    var nameContent = 
      if nameTag.value == "" 
      then nameTag.placeholder
      else nameTag.value
    
    
    val encode = js.URIUtils.encodeURIComponent
    httpReq.send(s"src=${encode(content)}&name=${encode(nameContent)}")
  }

  @JSExport
  def newBlogSkele(curId: Int): Unit = {
    val httpReq = new dom.XMLHttpRequest()
    // println(c·urId)
    httpReq.onreadystatechange = event =>
      if (httpReq.readyState == 4 && httpReq.status == 200)
      then {
        val outputArea = document.getElementById(s"output$curId")
        outputArea.innerHTML = httpReq.responseText
      }
    httpReq.open("POST", "/submit", true)
    // httpReq.setRequestHeader("Content-type", "application/x-www-form-urlencoded")
    val content = document
      .getElementById(s"src$curId")
      .asInstanceOf[HTMLTextAreaElement]
      .value
    val nameTag = document
      .getElementById(s"name$curId")
      .asInstanceOf[HTMLTextAreaElement]
    var nameContent = 
      if nameTag.value == "" 
      then nameTag.placeholder
      else nameTag.value
    val titleContent =
      document
      .getElementById(s"title$curId")
      .asInstanceOf[HTMLTextAreaElement]
      .value
    
    
    val encode = js.URIUtils.encodeURIComponent
    httpReq.send(
      s"src=${encode(content)}&name=${encode(nameContent)}&title=${encode(titleContent)}"
    )
  }

  @JSExport
  var mainContentWideMode: Boolean = false

  @JSExport
  def changeMainContentMode(): Unit = {
    if mainContentWideMode 
    then decreaseMainContent()
    else widenMainContent()

    mainContentWideMode = !mainContentWideMode
  }

  @JSExport
  def widenMainContent(): Unit = {
    import scalajs.js.internal.UnitOps.unitOrOps
    document.querySelector(".blog-content")
      .asInstanceOf[html.Element]
      .style
      .marginLeft = "3%"
      
    document.querySelector(".blog-content")
      .asInstanceOf[html.Element]
      .style
      .marginRight = "3%"
  }

  @JSExport
  def decreaseMainContent(): Unit = {
    import scalajs.js.internal.UnitOps.unitOrOps
    document.querySelector(".blog-content")
      .asInstanceOf[html.Element]
      .style
      .marginLeft = "20%"
      
    document.querySelector(".blog-content")
      .asInstanceOf[html.Element]
      .style
      .marginRight = "20%"
  }
  // js.timers.setTimeout(100)(changeMainContentMode())
  
  @JSExport
  def expandTextArea(obj: HTMLTextAreaElement): Unit = {
    obj.style.height = s"${obj.style.height.toInt + 1}"
  }


  // var textarea = document.querySelector("textarea");

  // textarea.addEventListener("keydown", autosize);
  
  // @JSExport
  // def autosize() = {
  //   var el = this
  //   js.timers.setTimeout(() => {
  //     el.style.cssText = "height:auto; padding:0"
  //     // for box-sizing other than "content-box" use:
  //     // el.style.cssText = '-moz-box-sizing:content-box';
  //     el.style.cssText = "height:" + el.scrollHeight + "px"
  //   }, 0)
  // }

  // const tx = document.getElementsByTagName("textarea");
  // for (let i = 0; i < tx.length; i++) {
  //   tx[i].setAttribute("style", "height:" + (tx[i].scrollHeight) + "px;overflow-y:hidden;");
  //   tx[i].addEventListener("input", OnInput, false);
  // }

  // function OnInput() {
  //   this.style.height = "auto";
  //   this.style.height = (this.scrollHeight) + "px";
  // }

  // import js.given
  def activeTextareaAutoExpand(): Unit = {
    val tx = document.getElementsByTagName("textarea")
    for i <- 0 until tx.length do
      val cur = tx(i).getAttribute("style")
      tx(i).setAttribute("style", cur + "height:" + (tx(i).scrollHeight) + "px;overflow-y:hidden;")
      tx(i).addEventListener("input", OnInput, false)

    def OnInput(event: dom.Event) = {
      val self = event.target.asInstanceOf[html.Element]
      self.style.height = "auto"
      self.style.height = (self.scrollHeight) + "px"
    }
  }

  activeTextareaAutoExpand()


end Main



















// import scala.scalajs.js.annotation.JSExport
// import org.scalajs.dom
// import scala.util.Random
// import scala.scalajs.js.annotation.JSExportTopLevel

// case class Point(x: Int, y: Int){
//   def +(p: Point) = Point(x + p.x, y + p.y)
//   def /(d: Int) = Point(x / d, y / d)
// }


// object ScalaJSExample {
    
//   def draw(canvas: html.Canvas): Unit = {
//     val renderer = canvas.getContext("2d")
//       .asInstanceOf[dom.CanvasRenderingContext2D]

//     canvas.width = canvas.parentElement.clientWidth
//     canvas.height = canvas.parentElement.clientHeight

//     renderer.fillStyle = "#f8f8f8"
//     renderer.fillRect(0, 0, canvas.width, canvas.height)

//     renderer.fillStyle = "black"
//     var down = false
//     canvas.onmousedown =
//       (e: dom.MouseEvent) => down = true

//     canvas.onmouseup =
//       (e: dom.MouseEvent) => down = false

//     canvas.onmousemove = (e: dom.MouseEvent) => {
//       val rect =
//       canvas.getBoundingClientRect()
//       if (down) renderer.fillRect(
//         e.clientX - rect.left,
//         e.clientY - rect.top,
//         10, 10
//       )
//     }
//   }

//   def clock(canvas: html.Canvas): Unit = {
//     val renderer = canvas.getContext("2d")
//       .asInstanceOf[dom.CanvasRenderingContext2D]

//     canvas.width = canvas.parentElement.clientWidth
//     canvas.height = canvas.parentElement.clientHeight

//     val gradient = renderer.createLinearGradient(
//       canvas.width / 2 - 100, 0, canvas.width/ 2 + 100, 0
//     )
//     gradient.addColorStop(0,"red")
//     gradient.addColorStop(0.5,"green")
//     gradient.addColorStop(1,"blue")
//     renderer.fillStyle = gradient
//     //renderer.fillStyle = "black"

//     renderer.textAlign = "center"
//     renderer.textBaseline = "middle"

//     def render() = {
//       val date = new js.Date()
//       renderer.clearRect(
//         0, 0, canvas.width, canvas.height
//       )

//       renderer.font = "75px sans-serif"
//       renderer.fillText(
//         Seq(
//           date.getHours(),
//           date.getMinutes(),
//           date.getSeconds()
//         ).mkString(":"),
//         canvas.width / 2,
//         canvas.height / 2
//       )
//     }
//     dom.window.setInterval(render _, 1000)
//   }
// }