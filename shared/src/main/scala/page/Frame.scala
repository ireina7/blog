package blog.page


import scalatags.Text.all.{
    title => titleAttr,
    _
}
import scalatags.Text.tags2.title
// import scalatags.Text.short._


object Frame {
    import scalatags.Text
    import scalatags.Text.TypedTag
    type HTML = String
    
    val bootStrap = link(
        href := "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
        rel := "stylesheet",
        attr("integrity") := "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
        attr("crossorigin") := "anonymous",
    )

    val highlight = Seq(
        link(
            rel := "stylesheet",
            href := "/assets/css/highlight/styles/a11y-light.min.css",
        ),
        script(src := "/assets/css/highlight/highlight.min.js"),
    )

    def index(mainContent: TypedTag[HTML] = div()): TypedTag[HTML] = {
        html (
            head(
                title("Ireina's magic"),
                meta(attr("http-equiv") := "Content-Type", content := "text/html; charset=UTF-8"),
                link(rel := "stylesheet", href := "/assets/css/main.css"),
                bootStrap,
                highlight,
            ),
            body(
                navBar, 
                br, hr,
                div(id := "content")(mainContent), // entry
                br, hr,
                footer,
                script(`type` := "text/javascript", src := "/assets/js/main.js"),
                script("hljs.highlightAll();"),
            ),
        )
    }



    val searchBar: TypedTag[HTML] = {

        form(cls := "d-flex", marginLeft := 45, bottom := 0, marginTop := 30, width := 600)(
            input(
                `class` := "form-control me-2", 
                `type` := "search",
                height := 40,
                placeholder := "order", 
                attr("aria-label") := "Search"
            ),
            button(`class` := "btn btn-outline-success", `type` := "submit", height := 40)("summon")
        )
    }

    val navBar: TypedTag[HTML] = {

        div(
            backgroundImage := "url(/assets/img/yuii.jpg)", 
            backgroundPosition := "right bottom",
            backgroundSize := "cover",
        )(
            tag("nav")(`class` := "nav sticky-top", marginTop := 0, marginLeft := 50, height := 150)(
                div(marginTop := 20)(
                    a(
                        `class` := "navbar-brand",
                        href := "#",
                        // onclick := { () => if (!sideBarShow) openSideBar() else closeSideBar() }
                    )(
                        img(`class` := "spinner", src := "/assets/img/lambda-icon-18.jpg", width := 70),
                    ),
                    span(`class` := "navbar-text", fontSize := 30)(i(b(span(color := "purple")("Ireina's "), "magic"))),
                ),
                i(fontSize := 20)(b("TM"))
                // searchBar,
            ),
            searchBar,
            pre(
                marginLeft := 55,
                fontSize := 18,
            )(
                """
Y = λf. (λx. f (x x)) (λx. f (x x))

trait Monad[M[_]] {
    def pure[A](a: A): M[A]
    extension [A](ma: M[A]) def flatMap[B](f: A => M[B]): M[B]
}
                """
            )
        )
    }
    
    val footer: TypedTag[HTML] = {

        tag("footer")(`class` := "text-lg-start", marginLeft := 30)(
            div(`class` := "p-1")(
                p(
                    i("Powered by "), 
                    span(color := "red")(i(b("Scala."))), span(color := "orange")(i(b("js")))
                )
            ),
        )
    }


    def item(
        title: String,
        author: String,
        date: java.util.Date,
        view: TypedTag[HTML]
    ): TypedTag[HTML] = {
        
        div(`class` := "item")(
            h3(title),
            p(author),
            view,
        )
    }

}