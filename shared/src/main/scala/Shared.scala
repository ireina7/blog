package blog

import scalatags.Text.TypedTag


object Shared {

  val msg = "Shared message"
  val assetsPath = "./shared/assets"
  val snapshots = s"$assetsPath/snapshots.json"

  def generateHtml(): Unit = {
    println("blog> Generating static html file...")
    static.Generator.writeHtmlToFile("./shared/public/index.html")
    println("blog> Done.")
  }
}

type HtmlText = TypedTag[String]
type BlogContext[T] = blog.Configuation ?=> T

trait ToHTML[A] {
  extension (a: A) def toHTML: HtmlText
}
