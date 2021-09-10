package blog.shared


@main def main(): Unit = {
  println("Testing shared module...")
  val html = blog.static.Generator.generateHtml()
  println(html.toString)
}