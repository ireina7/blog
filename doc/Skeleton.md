# Guide of the Skeleton programming language


## Getting started
```
Hello world!
```
(may be the simplest `hello world` in this world... ;)


## Features
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
\f {
  x y z o...
}
```
```
\f{x}{y}{z} {o...}
```

### Variables and definitons
In `Skeleton`, all abstraction are achieved by a special function `\set`.
To `\set` a variable, write
```
\set {<variable name>} {<variable value>}
```

To `\set` a function, try
```
\set {<function pattern>} {<function body>}
```

For example:
```
\set {\css{\style}} {
  \lambda{\self} {
    font{\style}{\self}
  }
}
```

### Common utility functions




#### Example
```
\document {
  \set {\class} {article}

  \## {Head titles, the more stars, the less weigth of font}
  \paragraph {
    This is a paragraph.
    You can make (\italic texts italic) or (\bold texts).
  }

  \### {Lets insert image!}
  \image {
    (\set \src ./assets/k-on.png)
  }

  \### {Or a link}
  (\link (\set \href www.github.com) Github)
}
```

- To see more examples, please check directory `./skeleton/scripts`.