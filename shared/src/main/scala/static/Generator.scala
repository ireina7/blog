package blog.static

import blog.*
import blog.page.Frame


object Generator {

  given blog.Configuation = blog.Configuation.staticBlog
  
  def generateHtml(): HtmlText = {
    val index = Frame.index()
    index
  }

  /**
   * This is dirty!
  */
  def writeToFile(path: String, content: String): Unit = {
    import java.io.PrintWriter
    Some(new PrintWriter(path)).foreach { p => 
      p.write(content)
      p.close
    }
  }

  def writeHtmlToFile(path: String): Unit = {
    writeToFile(path, generateHtml().toString)
  }

}
