package sandbox.casestudies.crdt

import cats.kernel.CommutativeMonoid
import cats.implicits._

final case class GCounter[A](counters: Map[String, A]) {
  def increment(machine: String,
                amount: A)(implicit m: CommutativeMonoid[A]) = {
    val value = amount |+| counters.getOrElse(machine, m.empty)
    GCounter(counters + (machine -> value))
  }

  def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] =
    GCounter(this.counters |+| that.counters)

  def total(implicit m: CommutativeMonoid[A]): A =
    counters.values.toList.combineAll
}
