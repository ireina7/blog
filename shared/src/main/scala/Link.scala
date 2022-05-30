package blog

// Test to simulate static path link
trait Link

object Link:

  val root = new Link {
    val shared = new Link {
      val public = new Link {}
    }
  }

end Link
