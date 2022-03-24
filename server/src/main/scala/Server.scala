package blog.server

import cats.effect.{ ConcurrentEffect, ContextShift, Timer }
import cats.implicits.*
import fs2.Stream
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits.*
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import scala.concurrent.ExecutionContext.global


/** This trait may be used to implement classes from Java.
 * @tparam F[_] the effect
 * @tparam Server the server type
*/
trait BlogServer[F[_], Server]:
  extension (server: Server)
    def bind(ip: BlogServer.IP, port: Int): F[Unit]
    def serve(): F[Unit]

end BlogServer




object BlogServer:

  opaque type IP = String
  extension (ip: IP)
    def ipV4(a: Int, b: Int, c: Int, d: Int): IP = ???
    def ipV6(a: Int, b: Int, c: Int, d: Int): IP = ???

  
  
  // def stream[F[_]: ConcurrentEffect]
  //   (using T: Timer[F], C: ContextShift[F]): Stream[F, Nothing] = {
    
  //   val httpApp = Routes.mainRoutes[F].orNotFound
  //   val finalHttpApp = Logger.httpApp(true, true)(httpApp)
  //   for {
  //     exitCode <- BlazeServerBuilder[F](global)
  //       .bindHttp(8080, "0.0.0.0")
  //       .withHttpApp(finalHttpApp)
  //       .serve
  //   } yield exitCode
  // }.drain
    
end BlogServer



// Request => F[Respond]



