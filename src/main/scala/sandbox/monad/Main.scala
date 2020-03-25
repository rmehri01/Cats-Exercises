package sandbox.monad

import cats.Eval
import cats.data.{Reader, Writer}
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

  // Reader exercise
  case class Db(usernames: Map[Int, String], passwords: Map[String, String])
  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))
  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    findUsername(userId).flatMap(_ match {
      case Some(value) => checkPassword(value, password)
      case None        => false.pure[DbReader]
    })

  val users = Map(1 -> "dade", 2 -> "kate", 3 -> "margo")
  val passwords =
    Map("dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret")
  val db = Db(users, passwords)
  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(4, "davinci").run(db))

  // their solution with for comprehension
  def checkLoginWithFor(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      passwordOk <- username
        .map { username =>
          checkPassword(username, password)
        }
        .getOrElse {
          false.pure[DbReader]
        }
    } yield passwordOk

  // State exercise
  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num.toInt)
  }

  def operand(num: Int) = State[List[Int], Int] { oldStack =>
    val newStack = num :: oldStack
    (newStack, num)
  }

  def operator(function: (Int, Int) => Int) = State[List[Int], Int] {
    case b :: a :: tail =>
      val result = function(a, b)
      (result :: tail, result)
    case _ =>
      sys.error("Failed computation!")
  }

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans
  println(program.runA(Nil).value)

  def evalAll(input: List[String]): CalcState[Int] = {
    val calcStateList = input.map(evalOne)
    calcStateList.reduce((acc, nextState) => acc.flatMap(_ => nextState))
  }

  val program2 = evalAll(List("1", "2", "+", "3", "*"))
  println(program2.runA(Nil).value)

  // their solution
  def evalAllSol(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (a, b) =>
      a.flatMap(_ => evalOne(b))
    }

  def evalInput(input: String): Int = {
    val symbols = input.split(" ").toList
    evalAll(symbols).runA(Nil).value
  }

  println(evalInput("1 2 + 3 * 5 +"))
}
