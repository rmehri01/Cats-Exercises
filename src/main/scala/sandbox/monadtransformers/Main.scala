package sandbox.monadtransformers

import cats.data.EitherT
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Main extends App {
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map("Jazz" -> 6, "Bumblebee" -> 8, "Hot Rod" -> 10)

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(level) => EitherT.right(Future(level))
      case None        => EitherT.left(Future(s"$autobot is unreachable!"))
    }

  val result1 = Future.sequence(
    Seq(getPowerLevel("Bumblebee").value, getPowerLevel("James").value)
  )
  println(Await.result(result1, 1.second))

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      pl1 <- getPowerLevel(ally1)
      pl2 <- getPowerLevel(ally2)
    } yield (pl1 + pl2) > 15

  val result2 = Future.sequence(
    Seq(
      canSpecialMove("Bumblebee", "Hot Rod").value,
      canSpecialMove("Bumblebee", "James").value
    )
  )
  println(Await.result(result2, 1.second))

  def tacticalReport(ally1: String, ally2: String): String = {
    val specialMove = canSpecialMove(ally1, ally2).value

    Await.result(specialMove, 1.second) match {
      case Left(value) => s"Comms error: $value"
      case Right(value) =>
        if (value) s"$ally1 and $ally2 are ready to roll out!"
        else s"$ally1 and $ally2 need a recharge."
    }
  }

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Bumblebee", "Hot Rod"))
  println(tacticalReport("Jazz", "Ironhide"))
}
