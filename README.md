# λ Blog
My personal blog written mostly in Scala and Java.
Trying `finally tagless` with `extensive effects`.
![k-on!](./img/k-on-q.png)  
- version: `0.0.4`

## Dependences
- `Scala3` the programming language
- `Java` the programming language for better performence
- `sbt` the build tool
- `cats` and `cats-effect` for core
- `scala.js` and `scalatags` for front end
- `circe` for *Json* processing
- `http4s`, `doobie` and `quill` for back end


## Configuration
- Please refer to [configuration file](./doc/Configuration.md)

## Features
### The **Skeleton** programming language
#### Variable
```
\variable-name
```
#### Function application
```
(\f x y z o...)
```
It's pretty useful that you can write any part of parameters inside a pair of `{}`.
For example, the example code above can be rewritten as:
```
(\f x) {
  y z o...
}
```
or even
```
(\f) {
  x y z o...
}
```
#### Example
```
(\document) {
  (\set class article)

  (\** Head titles, the more stars, the less weigth of font)
  (\paragraph) {
    This is a paragraph.
    You can make (\italic texts italic) or (\bold texts).
  }

  (\*** Lets insert image!)
  (\image (\set src ./assets/k-on.png))

  (\*** Or a link)
  (\link (\set href www.github.com) Github)
}
```

### The HTTP server

### Highly extensible and scalable by using finally tagless
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