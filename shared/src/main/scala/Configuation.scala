package blog



enum BlogType:
  case Online
  case Static

case class Configuration(
  /** Blog type: static | online */
  blogType: BlogType,

  /** prompt content */
  prompt: String = "blog> ",

  /** Path configurations */
  path: Path = Path.default,
)


import BlogType.*

extension (conf: Configuration)

  def blogPath: String = conf.blogType match
    case Static => s"${conf.path.data}/staticPackage"
    case Online => s"${conf.path.data}/public"

  def indexPath: String = conf.blogType match
    case Static => s"${conf.path.data}/public/items.json"
    case Online => s"${conf.path.data}/public/items.json"

  def staticPath: String = 
    s"${conf.path.data}/staticPackage"
  
  def blogRoute: String = conf.blogType match
    case Static => s"."
    case Online => s"/blog"

  def assetsRoute: String = conf.blogType match
    case Static => "../../assets" 
    case Online => s"/assets"
  
end extension



object Configuration:
  
  val staticBlog = Configuration(blogType = Static)
  val onlineBlog = Configuration(blogType = Online)

end Configuration


