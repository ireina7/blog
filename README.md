# λ Blog
My personal blog written mostly in Scala and Java.
Trying `finally tagless` with `extensive effects`.
![k-on!](./img/k-on-q.png)  
- version: `0.4.2`

## Dependences
- `Scala3` the programming language
- `Java` the programming language for better performence
- `JVM` the runtime
- `sbt` the build tool
- `cats` and `cats-effect` for core
- `scala.js` and `scalatags` for front end
- `circe` for *Json* processing
- `http4s`, `doobie` and `quill` for back end


## Configuration
- Please refer to the [configuration file](./doc/Configuration.md)

## Features
### The **Skeleton** programming language
- Please refer to the [Skeleton guide](./doc/Skeleton.md)
- The Skeleton online repl! You can try Skeleton directly on web page!
  Check [Skeleton notebook](http://ireina.us/skeleton)

## Development
- Please refer to the [Development log](./doc/DEV.md)

### The HTTP server
The HTTP server is modeled as a *simple function*:
`Request[F] => OptionT[F, Response[F]]`
where `F` is the *computation effect*.

### Highly extensible and scalable by using finally tagless
All trait are parameterized with *computation effects* (e.g. `F[_]`).
One can easily extend more implementations of 
the program **without** changing any existed code.

### Dependency Injection
Using `The ReaderT pattern`.

#### Static blog generation
Just `given` the static blog configuration, then everything done!
```scala
given blog.Configuration = Configuration.staticBlog
```
#### Online dynamic blog generation
```scala
given blog.Configuration = Configuration.onlineBlog
```

## Usage
### Server mode
0. `cd <this folder>`
1. `sbt`
2. `compile`
3. `server/run`
4. Go to [http://localhost:8080/](http://localhost:8080/)

### Static Engine mode
0. `cd <this folder>`
1. `sbt`
2. `compile`
3. `skeleton/run register <skeleton script file path> <new blog title>`