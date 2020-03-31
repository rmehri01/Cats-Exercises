package sandbox.semigroupal

import cats._
import cats.data.Validated
import cats.implicits._

object Main extends App {
  // Semigroupal exercise
  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    for {
      a <- x
      b <- y
    } yield (a, b)

  println(product(List(1, 2), List(3, 4)))

  type ErrorOr[A] = Either[Vector[String], A]

  println(
    product[ErrorOr, Int, Int](Left(Vector("Error 1")), Left(Vector("Error 2")))
  )

  // Validated exercise
  case class User(name: String, age: Int)

  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def createUser(form: FormData): FailSlow[User] =
    (readName(form).toValidated, readAge(form).toValidated).mapN(User.apply)

  def readName(form: FormData): FailFast[String] =
    for {
      name <- getValue("name")(form)
      result <- nonBlank(name)
    } yield result

  def readAge(form: FormData): FailFast[Int] =
    for {
      age <- getValue("age")(form)
      ageInt <- parseInt(age)
      result <- nonNegative(ageInt)
    } yield result

  def getValue(field: String)(form: FormData): FailFast[String] =
    form.get(field).toRight(List(s"Cannot find field: $field"))

  def parseInt(str: String): FailFast[Int] =
    Either
      .catchOnly[NumberFormatException](str.toInt)
      .leftMap(e => List(s"Could not parse '$str' to an Int"))

  // ensure would be better to use here
  def nonBlank(input: String): FailFast[String] =
    if (input.nonEmpty) {
      input.asRight
    } else {
      List("The input was blank").asLeft
    }

  def nonNegative(input: Int): FailFast[Int] =
    if (input >= 0) {
      input.asRight
    } else {
      List(s"The input $input is negative").asLeft
    }

  println(createUser(Map("name" -> "gabe", "age" -> "23")))
  println(createUser(Map("name" -> "", "age" -> "-23")))
  println(createUser(Map("name" -> "", "age" -> "foo")))
}
