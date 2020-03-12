package sandbox.monoidandsemigroup

import cats._
import cats.implicits._

object Main extends App {

  // Monoids for Boolean
  implicit val booleanMonoidAnd: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def empty: Boolean = true

      override def combine(x: Boolean, y: Boolean): Boolean = x && y
    }

  implicit val booleanMonoidOr: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def empty: Boolean = false

      override def combine(x: Boolean, y: Boolean): Boolean = x || y
    }

  implicit val booleanMonoidExclusiveOr: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def empty: Boolean = false

      override def combine(x: Boolean, y: Boolean): Boolean = x ^ y
    }

  // didn't think of this one!
  implicit val booleanMonoidExclusiveNor: Monoid[Boolean] =
    new Monoid[Boolean] {
      override def empty: Boolean = true

      override def combine(x: Boolean, y: Boolean): Boolean = !(x ^ y)
    }

  // Monoids and Semigroups for Sets
  implicit def monoidConcat[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      override def empty: Set[A] = Set.empty[A]

      // could also use union
      override def combine(x: Set[A], y: Set[A]): Set[A] = x ++ y
    }

  implicit def semigroupIntersect[A]: Semigroup[Set[A]] =
    (x: Set[A], y: Set[A]) => x intersect y

  // didn't think of this one either!
  implicit def symDiffMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      override def empty: Set[A] = Set.empty[A]

      override def combine(x: Set[A], y: Set[A]): Set[A] =
        (x diff y) union (y diff x)
    }

  // adding exercise
  final case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid: Monoid[Order] =
    new Monoid[Order] {
      override def empty: Order = Order(0, 0)

      override def combine(x: Order, y: Order): Order =
        Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
    }

  def add[A](items: List[A])(implicit monoid: Monoid[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)

  println(add(List(1, 2, 3, 10)))
  println(add(List(1.some, 2.some, 3.some, None)))
  println(add(List(Order(1, 1), Order(2, 3), Order(0, 0))))
}
