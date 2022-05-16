package blog.server.data


final case class LoginMessage(
  userName: String,
  password: String,
)
