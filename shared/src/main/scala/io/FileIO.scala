package blog


trait FileIO[F[_], Path, Content] {
  
  def readFile(path: Path): F[String]
  def writeFile(path: Path, content: Content): F[Unit]
}


object FileIO:
  import cats.*
  import cats.effect.*

  // The dirty one
  given FileIO[Id, String, String] with {

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

  given FileIO[Id, java.nio.file.Path, String] with {

    def readFile(path: java.nio.file.Path) = {
      import java.nio.file.{ Files, Paths }
      new String(Files.readAllBytes(path))
    }
    def writeFile(path: java.nio.file.Path, content: String) = {
      import java.io.PrintWriter
      Some(new PrintWriter(path.toString)).foreach { p => 
        p.write(content)
        p.close
      }
    }
  }

  given FileIO[IO, String, String] with {

    def readFile(path: String) = 
      IO { summon[FileIO[Id, String, String]].readFile(path) }
    def writeFile(path: String, content: String) = 
      IO { summon[FileIO[Id, String, String]].writeFile(path, content) }
  }

end FileIO
