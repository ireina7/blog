package blog

import scalatags.Text.TypedTag


object Shared {

  val msg = "Shared message"
  val assetsPath = "./shared/assets"
  val snapshots = s"$assetsPath/snapshots.json"
}


trait ToHTML[A] {
  extension (a: A) def toHTML: TypedTag[String]
}