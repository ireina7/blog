package blog


case class Configuation(
  blogType: BlogType,
)

object Configuation {
  
  val staticBlog = Configuation(blogType = BlogType.Static)
  val onlineBlog = Configuation(blogType = BlogType.Online)
}


enum BlogType {
  case Online
  case Static

  def assetsPath: String = this match
    case Online => "/assets"
    case Static => "./assets"
  
}
