package sandbox.fpinscala.monoid

import sandbox.fpinscala.propertytesting.{Gen, Prop}
import Prop._

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val tripleGen = for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z)

    // associativity
    forAll(tripleGen) {
      case (a1, a2, a3) =>
        m.op(a1, m.op(a2, a3)) == m.op(m.op(a1, a2), a3)
    } &&
    // identity
    forAll(gen) { a1 =>
      m.op(a1, m.zero) == a1 &&
      m.op(m.zero, a1) == a1
    }
  }
}
