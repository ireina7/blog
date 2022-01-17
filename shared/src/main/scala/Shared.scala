package blog

import scalatags.Text.TypedTag
import cats.Id
import cats.effect.*

object Shared {

  val msg = "Shared message"
  val assetsPath = "./shared/assets"
  val snapshots = s"$assetsPath/snapshots.json"
}

type HtmlText = scalatags.Text.Frag
type BlogContext[T] = blog.Configuation ?=> T

trait ToHTML[A] {
  extension (a: A) def toHTML: HtmlText
}
