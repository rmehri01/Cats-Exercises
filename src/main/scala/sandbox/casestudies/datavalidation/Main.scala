package sandbox.casestudies.datavalidation

import cats.data.{Kleisli, NonEmptyList}
import cats.implicits._

object Main extends App {

  type Errors = NonEmptyList[String]

  type Result[A] = Either[Errors, A]

  type Check[A, B] = Kleisli[Result, A, B]

  // Create a check from a function:
  def check[A, B](func: A => Result[B]): Check[A, B] =
    Kleisli(func)

  // Create a check from a Predicate:
  def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] =
    Kleisli[Result, A, A](pred.run)

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

  val splitEmail: Check[String, (String, String)] = check(str => {
    val splitArr = str.split("@")
    splitArr match {
      case Array(name, domain) => (name, domain).asRight
      case _                   => error("Email must contain a single @ character").asLeft
    }
  })

  val joinEmail: Check[(String, String), String] =
    check[(String, String), String] {
      case (name, domain) =>
        (checkName(name), checkDomain(domain)).mapN(_ + "@" + _)
    }

  val checkName = checkPred(longerThan(0))

  val checkDomain = checkPred(longerThan(2) and containsOnce('.'))

  val checkEmail = splitEmail andThen joinEmail

  val checkUsername = checkPred(longerThan(3) and alphanumeric)

  println(checkUsername("a"))
  println(checkUsername("foobar"))
  println(checkUsername(".';'"))
  println(checkUsername("_"))

  println(checkEmail("h"))
  println(checkEmail("h@gmail.com"))
  println(checkEmail("h@gmail"))
  println(checkEmail("h@co"))

  final case class User(username: String, email: String)
  def createUser(username: String, email: String): Result[User] =
    (checkUsername(username), checkEmail(email)).mapN(User)
}
