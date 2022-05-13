package blog


case class Configuration(
  blogType: BlogType,
  prompt: String = "blog",
  dataPath: String = "./shared",
):
  def blogPath: String = blogType match
    case BlogType.Static => s"$dataPath/staticPackage"
    case BlogType.Online => s"$dataPath/public"
  
end Configuration


object Configuration {
  
  val staticBlog = Configuration(blogType = BlogType.Static)
  val onlineBlog = Configuration(blogType = BlogType.Online)
}


enum BlogType {
  case Online
  case Static

  def blogPath: String = this match
    case Static => s"."
    case Online => s"/blog"

  def assetsPath: String = this match
    case Online => s"/assets"
    case Static => "../assets"  
}
