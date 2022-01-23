package blog.core



trait DiskIO[F[_], Path, -Content, File]:
  def readFile(file: File): F[String]
  def writeFile(file: File, content: Content): F[Unit]

  def openFile(path: Path): F[File]
  def closeFile(file: File): F[Unit]
end DiskIO


import scala.io.{ Source, BufferedSource }

trait FileIO[F[_], Path, -Content] {
  
  def readFile(path: Path): F[String]
  def writeFile(path: Path, content: Content): F[Unit]
}


type FileIOString[F[_]] = 
  FileIO[F, String, String]




object FileIO:
  import cats.*
  import cats.effect.*
  import java.io.File

  // The dirty one
  // type Id[A] = A
  given FileIO[Id, String, String] with {

    override def readFile(path: String) = {
      import java.nio.file.{ Files, Paths }
      new String(Files.readAllBytes(Paths.get(path)))
    }
    override def writeFile(path: String, content: String) = {
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

  given (using rawIO: FileIO[Id, String, String]): 
    FileIO[IO, String, String] with {

    def readFile(path: String) = 
      IO { rawIO.readFile(path) }
    def writeFile(path: String, content: String) = 
      IO { rawIO.writeFile(path, content) }
    
  }

  export blog.core.Effect.given FileIO[?, ?, ?]

end FileIO
