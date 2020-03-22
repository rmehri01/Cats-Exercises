package sandbox.monad

import cats.Eval
import cats.data.Writer
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Main extends App {
  // Id exercise
  type Id[A] = A

  def pure[A](a: A): Id[A] = a

  def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)

  def map[A, B](value: Id[A])(func: A => B): Id[B] = func(value)

  println(pure(2))
  println(flatMap("foo")(_ + " bar"))

  // Either exercise
  case class User(username: String, password: String)
  type LoginResult = Either[LoginError, User]

  def handleError(error: LoginError): Unit =
    error match {
      case UserNotFound(u) =>
        println(s"User not found: $u")
      case PasswordIncorrect(u) =>
        println(s"Password incorrect: $u")
      case UnexpectedError =>
        println(s"Unexpected error")
    }

  val result1: LoginResult = User("dave", "passw0rd").asRight
  val result2: LoginResult = UserNotFound("dave").asLeft

  result1.fold(handleError, println)
  result2.fold(handleError, println)

  // Eval exercise
  def naiveFoldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, naiveFoldRight(tail, acc)(fn))
      case Nil =>
        acc
    }

  def safeFoldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    evalFoldRight(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value

  def evalFoldRight[A, B](as: List[A],
                          acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, evalFoldRight(tail, acc)(fn)))
      case Nil =>
        acc
    }

  //  naiveFoldRight((1 to 100000).toList, 0)(_ + _) – this stack overflows
  println(safeFoldRight((1 to 100000).toList, 0)(_ + _))

  // Writer exercise
  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  factorial(5)
  Await.result(
    Future.sequence(Vector(Future(factorial(3)), Future(factorial(3)))),
    5.seconds
  ) // logs are mixed up in multithreaded environments

  type Logged[A] = Writer[Vector[String], A]

  def writerFactorial(n: Int): Logged[Int] = {
    val ans = slowly(
      if (n == 0) 1.pure[Logged] else writerFactorial(n - 1).map(_ * n)
    )
    ans.mapWritten(log => log :+ s"fact $n ${ans.value}")
  }

  Await.result(
    Future.sequence(
      Vector(
        Future(println(writerFactorial(3).run)),
        Future(println(writerFactorial(3).run))
      )
    ),
    Duration.Inf
  )

  // their solution with for comprehension instead
  def writerFactorial2(n: Int): Logged[Int] =
    for {
      ans <- if (n == 0) { 1.pure[Logged] } else {
        slowly(writerFactorial2(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
}
