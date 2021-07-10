


case class Ref(address: String)
case class Tag(name: String)

trait Item[I] {
    def id: Int
    def ref: Ref
    def tags: Seq[Tag]
}


case class Article(val title: String)

object Item {
    implicit object given_article extends Item[Article] {
        def id = 0
        def ref = ???
        def tags = ???
    }
}