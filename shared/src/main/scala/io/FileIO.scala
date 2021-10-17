package blog


trait FileIO[F[_], Path, Content] {
  
  def readFile(path: Path): F[String]
  def writeFile(path: Path, content: Content): F[Unit]
}


object FileIO {
  import cats.*

  // The dirty one
  given blog.FileIO[Id, String, String] with {
    def readFile(path: String) = {
      import java.nio.file.{ Files, Paths }
      new String(Files.readAllBytes(Paths.get(path)))
    }
    def writeFile(path: String, content: String) = {
      import java.io.PrintWriter
      Some(new PrintWriter(path)).foreach { p => 
        p.write(content)
        p.close
      }
    }
  }

}
