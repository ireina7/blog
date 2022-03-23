package blog


case class Path(path: String)

object Path:

  val root = "."
  val assets = s"$root/shared/assets"
  val staticPackage = s"$root/shared/staticPackage"
  val blogs = s"$root/shared/public"
  val items = s"$root/shared/public/items.json"

end Path