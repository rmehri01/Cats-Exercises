package sandbox.fpinscala.propertytesting

import Prop._
import Gen._

object Main {
  def main(args: Array[String]): Unit = {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    run(maxProp)

    val sortProp = forAll(listOf(smallInt)) { ns =>
      val sorted = ns.sorted
      def inOrder(l: List[Int]): Boolean = l match {
        case x1 :: x2 :: xs => x1 <= x2 && inOrder(x2 :: xs)
        case _              => true
      }

      inOrder(sorted) &&
      ns.forall(sorted.contains(_)) &&
      sorted.forall(ns.contains(_))
    }
    run(sortProp)
  }
}
