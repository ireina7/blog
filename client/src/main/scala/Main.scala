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



@JSExportTopLevel("blog")
object Main {

  def main(args: Array[String]): Unit = {
    println("Hello world!")
    document.addEventListener("DOMContentLoaded", (e: dom.Event) => {
      setup()
    })
  }

  /**
   * It is sad that we cannot reuse [[blog.BlogType]] here.
  */
  val assetsPath = 
    if isStatic then "./assets" else "/assets"

  def setup(): Unit = {
    
    document.body.appendChild(sideBar)
    // if isStatic then
    //   setupStaticBlogIfNecessary()
    // else // is dynamic blog
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


  val content: dom.Element = {
    div(marginLeft := 50, marginRight := 50, fontSize := 20)(
      p("""
这是一个首页测试，目前问题还有很多，视觉性较差。@%%%%
我是Ireina，混迹在世界各地的旅行魔女，遇到我将会是你的荣幸。
      """),
      p("高亮解决！"),
      p("代码示例："),
      pre()(code(`class` := "language-scala")("""
def searchBar: dom.Element = {  

form(cls := "d-flex")(  
    input(
        `class` := "form-control me-2",
        `type` := "search", 
        placeholder := "search", 
        attr("aria-label") := "Search"
    ),
    button(`class` := "btn btn-outline-success", `type` := "submit")("search")
)
}.render
      """)),
    )
  }.render

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

    form(cls := "d-flex", marginLeft := 45, bottom := 0, marginTop := 30, width := 600)(
      input(
        `class` := "form-control me-2", 
        `type` := "search",
        height := 40,
        placeholder := "order", 
        attr("aria-label") := "Search"
      ),
      button(`class` := "btn btn-outline-success", `type` := "submit", height := 40)("summon")
    )
  }.render

  val sideBar: dom.Element = {

    div(id := "mySidenav", `class` := "sidenav")(
      a(href := "/")("Home"),
      a(href := "/about")("About"),
      a(href := "/filter")("Filter"),
      a(href := "#")("Category"),
      a(href := "#")("Structure"),
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
      backgroundImage := s"url($assetsPath/img/yuii.jpg)", 
      backgroundPosition := "right bottom",
      backgroundSize := "cover",
    )(
      tag("nav")(`class` := "nav sticky-top", marginTop := 0, marginLeft := 50, height := 150)(
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
        marginLeft := 55,
        fontSize := 18,
      )(
        """
Y = λf. (λx. f (x x)) (λx. f (x x))

trait Monad[M[_]] {
  def pure[A](a: A): M[A]
  extension [A](ma: M[A]) def flatMap[B](f: A => M[B]): M[B]
}
        """
      )
    )
  }.render

}//end Main






// import scala.scalajs.js.annotation.JSExport
// import org.scalajs.dom
import scala.util.Random
// import scala.scalajs.js.annotation.JSExportTopLevel

case class Point(x: Int, y: Int){
  def +(p: Point) = Point(x + p.x, y + p.y)
  def /(d: Int) = Point(x / d, y / d)
}


object ScalaJSExample {
    
  def draw(canvas: html.Canvas): Unit = {
    val renderer = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    canvas.width = canvas.parentElement.clientWidth
    canvas.height = canvas.parentElement.clientHeight

    renderer.fillStyle = "#f8f8f8"
    renderer.fillRect(0, 0, canvas.width, canvas.height)

    renderer.fillStyle = "black"
    var down = false
    canvas.onmousedown =
      (e: dom.MouseEvent) => down = true

    canvas.onmouseup =
      (e: dom.MouseEvent) => down = false

    canvas.onmousemove = (e: dom.MouseEvent) => {
      val rect =
      canvas.getBoundingClientRect()
      if (down) renderer.fillRect(
        e.clientX - rect.left,
        e.clientY - rect.top,
        10, 10
      )
    }
  }

  def clock(canvas: html.Canvas): Unit = {
    val renderer = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    canvas.width = canvas.parentElement.clientWidth
    canvas.height = canvas.parentElement.clientHeight

    val gradient = renderer.createLinearGradient(
      canvas.width / 2 - 100, 0, canvas.width/ 2 + 100, 0
    )
    gradient.addColorStop(0,"red")
    gradient.addColorStop(0.5,"green")
    gradient.addColorStop(1,"blue")
    renderer.fillStyle = gradient
    //renderer.fillStyle = "black"

    renderer.textAlign = "center"
    renderer.textBaseline = "middle"

    def render() = {
      val date = new js.Date()
      renderer.clearRect(
        0, 0, canvas.width, canvas.height
      )

      renderer.font = "75px sans-serif"
      renderer.fillText(
        Seq(
          date.getHours(),
          date.getMinutes(),
          date.getSeconds()
        ).mkString(":"),
        canvas.width / 2,
        canvas.height / 2
      )
    }
    dom.window.setInterval(render _, 1000)
  }
}