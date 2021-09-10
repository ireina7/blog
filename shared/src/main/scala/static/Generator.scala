package blog.static

import blog.*
import blog.page.Frame


object Generator {

  given blog.Configuation = blog.Configuation.staticBlog
  
  def generateHtml(): HtmlText = {
    val index = Frame.index()
    index
  }

}
