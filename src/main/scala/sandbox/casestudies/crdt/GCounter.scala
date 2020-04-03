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

  import KeyValueStore._

  implicit def gcounterInstance[F[_, _], K, V](implicit kvs: KeyValueStore[F],
                                               km: CommutativeMonoid[F[K, V]]) =
    new GCounter[F, K, V] {
      def increment(
        f: F[K, V]
      )(key: K, value: V)(implicit m: CommutativeMonoid[V]): F[K, V] = {
        val total = f.getOrElse(key, m.empty) |+| value
        f.put(key, total)
      }
      def merge(f1: F[K, V],
                f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] =
        f1 |+| f2
      def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V =
        f.values.combineAll
    }
}
