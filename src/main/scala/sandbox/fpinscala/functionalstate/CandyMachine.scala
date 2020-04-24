package sandbox.fpinscala.functionalstate

import State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class CandyMachine(locked: Boolean, candies: Int, coins: Int)

object CandyMachine {
  def update(i: Input)(s: CandyMachine): CandyMachine =
    (i, s) match {
      case (_, CandyMachine(_, 0, _)) => s
      case (Coin, CandyMachine(true, cd, co)) =>
        CandyMachine(locked = false, cd, co + 1)
      case (Turn, CandyMachine(false, cd, co)) =>
        CandyMachine(locked = true, cd - 1, co)
      case _ => s
    }

  def simulateMachine(inputs: List[Input]): State[CandyMachine, (Int, Int)] =
    for {
      _ <- sequence(inputs map (modify[CandyMachine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)

}
