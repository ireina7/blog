import sbt._

object Dependencies {
    
    val junit = "com.novocode" % "junit-interface" % "0.11"
    val scalaParallelCollection = "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3"
    val rxScala = "io.reactivex" %% "rxscala" % "0.27.0"
    val akka = "com.typesafe.akka" %% "akka-actor-typed" % "2.6.15"
}

object Operations {

    def copyFile(from: String, to: String): Unit = {
        import java.io.{ File,FileInputStream, FileOutputStream }
        
        val a = new File(from)
        val b = new File(to)
        new FileOutputStream(b)
            .getChannel()
            .transferFrom(new FileInputStream(a) getChannel, 0, Long.MaxValue)
    }
}
