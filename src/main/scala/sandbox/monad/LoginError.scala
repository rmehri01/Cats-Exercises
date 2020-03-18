package sandbox.monad

sealed trait LoginError extends Product with Serializable

final case class UserNotFound(username: String) extends LoginError
final case class PasswordIncorrect(username: String) extends LoginError
case object UnexpectedError extends LoginError
