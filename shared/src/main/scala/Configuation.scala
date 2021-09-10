package blog


case class Configuation(
  isStatic: Boolean,
)

object Configuation {
  
  val staticBlog = Configuation(isStatic = true)
  val onlineBlog = Configuation(isStatic = false)
}
