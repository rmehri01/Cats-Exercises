package sandbox.casestudies.datavalidation

import cats._
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.implicits._

sealed trait Predicate[E, A] {
  def apply(value: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Single(f) => f(value)

      case And(left, right) =>
        val res1 = left(value)
        val res2 = right(value)
        (res1, res2).mapN((_, _) => value)

      case Or(left, right) =>
        val res1 = left(value)
        val res2 = right(value)
        (res1, res2) match {
          case (Invalid(err1), Invalid(err2)) => (err1 |+| err2).invalid
          case _                              => value.valid
        }
    }

  def and(that: Predicate[E, A]): Predicate[E, A] =
    And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] =
    Or(this, that)
}

final case class Single[E, A](f: A => Validated[E, A]) extends Predicate[E, A]

final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A])
    extends Predicate[E, A]

final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A])
    extends Predicate[E, A]
