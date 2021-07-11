package page


import scalatags.Text.all.{
    title => titleAttr,
    _
}
import scalatags.Text.tags2.title
// import scalatags.Text.short._


object Frame {
    
    val bootStrap = link(
        href := "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
        rel := "stylesheet",
        attr("integrity") := "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
        attr("crossorigin") := "anonymous",
    )

    val highlight = link(
        ???
    )

    def index: scalatags.Text.TypedTag[String] = {
        html (
            head(
                title("Ireina's magic"),
                meta(attr("http-equiv") := "Content-Type", content := "text/html; charset=UTF-8"),
                link(rel := "stylesheet", href := "main.css"),
                bootStrap,
            ),
            body(
                div(id := "content"),
                script(`type` := "text/javascript", src := "main.js"),
                script("hljs.highlightAll();"),
            ),
        )
    }

}