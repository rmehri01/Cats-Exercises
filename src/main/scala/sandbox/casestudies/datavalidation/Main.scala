package sandbox.casestudies.datavalidation

import cats._
import cats.data.Validated
import cats.implicits._

object Main extends App {
  sealed trait Check[E, A] {
    def apply(value: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Single(f) => f(value)
        case And(left, right) => {
          val res1 = left.apply(value)
          val res2 = right.apply(value)
          (res1, res2).mapN((_, _) => value)
        }
      }

    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)
  }

  final case class Single[E, A](f: A => Validated[E, A]) extends Check[E, A]

  final case class And[E, A](left: Check[E, A], right: Check[E, A])
      extends Check[E, A]

  val single1 = Single[Vector[String], Int](
    value => if (value == 6) Vector("err1").invalid else value.valid
  )
  val single2 = Single[Vector[String], Int](
    value => if (value % 2 == 0) Vector("err2").invalid else value.valid
  )
  println((single1 and single2).apply(6))
}
