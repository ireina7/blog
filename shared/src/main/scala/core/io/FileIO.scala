package blog.core

import scala.util.{Try, Success, Failure}
import java.io.File



trait DiskIO[F[_], Path, -Content, File]:
  def readFile(file: File): F[String]
  def writeFile(file: File, content: Content): F[Unit]

  def openFile(path: Path): F[File]
  def createFile(path: Path): F[File]
  def closeFile(file: File): F[Unit]
end DiskIO


import scala.io.{ Source, BufferedSource }

trait FileIO[F[_], Path, -Content] {
  
  /** File operations 
   * Including:
      - creating file
      - reading file
      - writing file
      - check if file exists
  */
  def createFile(path: Path): F[Unit]
  def readFile  (path: Path): F[String]
  def writeFile (path: Path, content: Content): F[Unit]
  def existFile (path: Path): F[Boolean]

  /** Directory operations 
   * Including
   * - creating directory
   * - copy entire directory recursively
   * - check if directory exists
  */
  def createDirectory(path: Path): F[Unit]
  def copyDirectory(from: Path, to: Path): F[Unit]
  def existDirectory(path: Path): F[Boolean]
  // def writeDirectory (to: Path): F[Unit]
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

    override def createFile(path: String): Unit = {
      val file: File = new File(path)
      file.createNewFile() // if already exists, no warning though...
    }
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
    override def existFile(path: String): Boolean = {
      val f: File = new File(path);
      f.exists() && !f.isDirectory()
    }

    override def createDirectory(path: String): Unit = {
      val theDir: File = new File(path)
      if !theDir.exists() then
        theDir.mkdirs()
    }

    override def copyDirectory(from: String, to: String): Unit = {
      import java.nio.file.Paths
      summon[FileIO[Id, java.nio.file.Path, String]]
        .copyDirectory(Paths.get(from), Paths.get(to))
    }

    override def existDirectory(path: String): Boolean = {
      import java.nio.file.Files
      import java.nio.file.Paths
      Files.exists(Paths.get(path))
    }
    // override def writeDirectory (path: String): Unit = {
    //   ???
    // }
  }

  given FileIO[Id, java.nio.file.Path, String] with {

    def createFile(path: java.nio.file.Path): Unit = {
      val file: File = new File(path.toString)
      file.createNewFile() // if already exists, no warning though...
    }

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
    override def existFile(path: java.nio.file.Path): Boolean = {
      val f: File = new File(path.toString);
      f.exists() && !f.isDirectory()
    }
    override def createDirectory(path: java.nio.file.Path): Unit = {
      val theDir: File = new File(path.toString)
      if !theDir.exists() then
        theDir.mkdirs()
    }
    override def existDirectory(path: java.nio.file.Path): Boolean = {
      import java.nio.file.Files
      import java.nio.file.Paths
      Files.exists(path)
    }

    override def copyDirectory(from: java.nio.file.Path, to: java.nio.file.Path): Unit = {
      import java.nio.file.StandardCopyOption.*
      import java.nio.file.{Files, Path}
      var stream: java.util.stream.Stream[Path] = null
      deleteFolder(new File(to.toString))
      try {
        stream = Files.walk(from)
        stream.forEach {
          source => 
            copyFolder(source, to.resolve(from.relativize(source)))
        }
      } finally {
        stream.close()
      }
    }

    private def deleteFolder(directory: File): Boolean = {
      import java.io.{ 
        File,
        FileInputStream, 
        FileOutputStream,
        IOException,
      }

      if (directory.exists()) {
        val files = directory.listFiles()
        if (null != files) {
          for (i <- 0 until files.length) {
            if (files(i).isDirectory()) {
              deleteFolder(files(i));
            }
            else {
              files(i).delete();
            }
          }
        }
      }
      directory.delete()
    }
    private def copyFolder(from: java.nio.file.Path, to: java.nio.file.Path): Unit = {
      import java.nio.file.StandardCopyOption.*
      import java.nio.file.{Files, Path}
      Files.copy(from, to, REPLACE_EXISTING)
    }
    // override def writeDirectory (path: java.nio.file.Path): Unit = {
    //   ???
    // }
  }

  given [Path](using rawIO: FileIO[Id, Path, String])
    : FileIO[IO, Path, String] with 

    override def createFile(path: Path): IO[Unit] = 
      IO(Try(rawIO.createFile(path))).flatMap {
        case Success(_) => IO(())
        case Failure(e) => IO.raiseError(e)
      }

    override def readFile(path: Path) = 
      // IO { rawIO.readFile(path) }
      IO(Try(rawIO.readFile(path))).flatMap {
        case Success(s) => IO(s)
        case Failure(e) => IO.raiseError(e)
      }
    
    override def writeFile(path: Path, content: String) = 
      // IO { rawIO.writeFile(path, content) }
      IO(Try(rawIO.writeFile(path, content))).flatMap {
        case Success(_) => IO(())
        case Failure(e) => IO.raiseError(e)
      }

    override def existFile(path: Path): IO[Boolean] = 
      IO(Try(rawIO.existFile(path))).flatMap {
        case Success(b) => IO(b)
        case Failure(e) => IO.raiseError(e)
      }

    override def createDirectory(path: Path): IO[Unit] = 
      IO(Try(rawIO.createDirectory(path))).flatMap {
        case Success(_) => IO(())
        case Failure(e) => IO.raiseError(e)
      }

    override def copyDirectory(from: Path, to: Path): IO[Unit] = 
      IO(Try(rawIO.copyDirectory(from, to))).flatMap {
        case Success(_) => IO(())
        case Failure(e) => IO.raiseError(e)
      }
    override def existDirectory(path: Path): IO[Boolean] = 
      IO(Try(rawIO.existDirectory(path))).flatMap {
        case Success(b) => IO(b)
        case Failure(e) => IO.raiseError(e)
      }
  end given

  export blog.core.Effect.given FileIO[?, ?, ?]

end FileIO
