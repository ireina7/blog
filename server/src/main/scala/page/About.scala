package page

import scalatags.Text.all._
import scalatags.Text.tags2.section
import scalatags.Text.tags2.{title => mainTitle}

final case class LabelTagPair(label: String, tag: String)

object About {
  val labelTags = List(LabelTagPair("label-success", "Coding"), LabelTagPair("label-warning", "Scala"), LabelTagPair("label-primary", "Scalatags"),
    LabelTagPair("label-danger", "Cats"), LabelTagPair("label-info", "Javascript"))

  def main = html(
    head(
      mainTitle("About | Tutorial page"),
      link(rel := "stylesheet", href := "https://stackpath.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"),
      link(rel := "stylesheet", href := "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.8.2/css/all.min.css"),
      link(rel := "stylesheet", href := "http://code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css"),
      link(rel := "stylesheet", href := "https://adminlte.io/themes/AdminLTE/dist/css/AdminLTE.min.css"),
    ),
    body(
      section(cls := "content",
        div(cls := "row",
          div(cls := "col-md-9",

            div(cls := "box box-primary",
              div(cls := "box-body box-profile",
                img(cls := "profile-user-img img-responsive img-circle", src := "https://avatars2.githubusercontent.com/u/22730534?s=400&u=a8588b4a7fce501f31964ab8c666e32e0275748f&v=4", alt := "User profile picture"),
                h3(cls := "profile-username text-center", "Emmett the Machine"),
                p(cls := "text-muted text-center", "Software Engineer"),
                ul(cls := "list-group list-group-unbordered",
                  li(cls := "list-group-item", b("Followers", a(cls := "pull-right", "123,456"))),
                  li(cls := "list-group-item", b("Following", a(cls := "pull-right", "123,456"))),
                  li(cls := "list-group-item", b("Friends", a(cls := "pull-right", "123,456")))
                ),
                a(href := "https://github.com/emmettna/scalatags-example", cls := "btn btn-primary btn-block", b("Visit Github Page"))
              )
            ),
            div(cls := "box box-primary",
              div(cls := "box-header with-border",
                h3(cls := "box-title", "More About"))
            ),
            div(cls := "box-body",
              strong(i(cls := "fa fa-book margin-r-5"), "Education"),
              p(cls := "text-muted", "the best Kindergarten ever in the world."),
              hr,
              strong(i(cls := "fa fa-map-marker margin-r-5"), "Location"),
              p(cls := "text-muted", "Somewhere, Seoul"),
              hr,
              strong(i(cls := "fas fa-pencil-alt margin-r-5"), "Skills"),
              p(style := "margin:3px",
                labelTags.map { pair => span(cls := s"label ${pair.label} margin-r-3", pair.tag) }
              ),
              hr,
              strong(i(cls := "far fa-file-alt margin-r-5"), "Notes"),
              p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam fermentum enim neque.")
            )
          ),
        )
      ),
      script(src := "https://code.jquery.com/jquery-3.5.1.min.js"),
      script(src := "https://stackpath.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js"),
      script(src := "https://cdnjs.cloudflare.com/ajax/libs/fastclick/1.0.6/fastclick.min.js"),
      script(src := "https://cdnjs.cloudflare.com/ajax/libs/admin-lte/3.0.5/js/adminlte.min.js")
    ))
}