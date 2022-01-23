package blog


case class Configuration(
  blogType: BlogType,
  prompt: String = "blog",
)

object Configuration {
  
  val staticBlog = Configuration(blogType = BlogType.Static)
  val onlineBlog = Configuration(blogType = BlogType.Online)
}


enum BlogType {
  case Online
  case Static

  def assetsPath: String = this match
    case Online => "/assets"
    case Static => "./assets"
  
}
