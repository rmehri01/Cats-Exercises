package sandbox.casestudies.crdt

import cats.kernel.CommutativeMonoid
import cats.implicits._

trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(
    implicit m: CommutativeMonoid[V]
  ): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])(
    implicit b: BoundedSemiLattice[V]
  ): F[K, V]
  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

object GCounter {
  def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) =
    counter

  implicit def mapGCounter[A] = new GCounter[Map, String, A] {
    override def increment(
      f: Map[String, A]
    )(k: String, v: A)(implicit m: CommutativeMonoid[A]): Map[String, A] = {
      val value = v |+| f.getOrElse(k, m.empty)
      f + (k -> value)
    }

    override def merge(f1: Map[String, A], f2: Map[String, A])(
      implicit b: BoundedSemiLattice[A]
    ): Map[String, A] = f1 |+| f2

    override def total(f: Map[String, A])(implicit m: CommutativeMonoid[A]): A =
      f.values.toList.combineAll
  }
}
