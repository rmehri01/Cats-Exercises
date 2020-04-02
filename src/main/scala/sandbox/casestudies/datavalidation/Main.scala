package sandbox.casestudies.datavalidation

import cats.data.{NonEmptyList, Validated}
import cats.implicits._

object Main extends App {

  type Errors = NonEmptyList[String]
  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.size > n
    )

  val alphanumeric: Predicate[Errors, String] = Predicate.lift(
    error(s"Must be all alphanumeric characters"),
    str => str.forall(_.isLetterOrDigit)
  )

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char)
    )

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1
    )

  val splitEmail: Check[Errors, String, (String, String)] = Check(str => {
    val splitArr = str.split("@")
    splitArr match {
      case Array(name, domain) => (name, domain).valid
      case _                   => error("Email must contain a single @ character").invalid
    }
  })

  val joinEmail: Check[Errors, (String, String), String] =
    Check[Errors, (String, String), String] {
      case (name, domain) =>
        (checkName(name), checkDomain(domain)).mapN(_ + "@" + _)
    }

  val checkName = Check(longerThan(0))

  val checkDomain = Check(longerThan(2) and containsOnce('.'))

  val checkEmail: Check[Errors, String, String] = splitEmail andThen joinEmail

  val checkUsername: Check[Errors, String, String] = Check(
    longerThan(3) and alphanumeric
  )
  
  println(checkUsername("a"))
  println(checkUsername("foobar"))
  println(checkUsername(".';'"))
  println(checkUsername("_"))

  println(checkEmail("h"))
  println(checkEmail("h@gmail.com"))
  println(checkEmail("h@gmail"))
  println(checkEmail("h@co"))

  final case class User(username: String, email: String)
  def createUser(username: String, email: String): Validated[Errors, User] =
    (checkUsername(username), checkEmail(email)).mapN(User)
}
