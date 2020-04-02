package sandbox.casestudies.datavalidation

import cats.data.Validated
import cats.kernel.Semigroup

sealed trait Check[E, A, B] {
  import Check._

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](func: B => C): Check[E, A, C] = MapCheck[E, A, B, C](this, func)

  def flatMap[C](func: B => Check[E, A, C]) =
    FlatMapCheck[E, A, B, C](this, func)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] = AndThen(this, that)
}

object Check {
  final case class AndThen[E, A, B, C](check1: Check[E, A, B],
                                       check2: Check[E, B, C])
      extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check1(a).withEither(_.flatMap(b => check2(b).toEither))
  }

  final case class MapCheck[E, A, B, C](check: Check[E, A, B], func: B => C)
      extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).map(func)
  }

  final case class FlatMapCheck[E, A, B, C](check: Check[E, A, B],
                                            func: B => Check[E, A, C])
      extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(func(_)(a).toEither))
  }

  final case class Pure[E, A, B](func: A => Validated[E, B])
      extends Check[E, A, B] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] =
      func(a)
  }

  final case class PurePredicate[E, A](pred: Predicate[E, A])
      extends Check[E, A, A] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      pred(a)
  }

  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
    PurePredicate(pred)

  def apply[E, A, B](func: A => Validated[E, B]): Check[E, A, B] =
    Pure(func)

}
