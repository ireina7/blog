package blog


abstract class Path:
  def root: String

  /** Path to store data, including: 
   * + online blog assets and pages
   * + static blog package
  */
  def data: String
  def index: String


object Path:

  val default = new Path {
    def root = s"."
    def data = s"$root/shared"
    def index = s"$data/public/items.json"
  }

  val root = default.root
  val assets = s"$root/shared/assets"
  val staticPackage = s"$root/shared/staticPackage"
  val blogs = s"$root/shared/public"
  val items = default.index

end Path