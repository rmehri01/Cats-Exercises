package sandbox.fpinscala.propertytesting

import Prop._
import Gen._
import sandbox.fpinscala.parallel.Par

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

    val mapProp = checkPar { equal(Par.map(Par.unit(1))(_ + 1), Par.unit(2)) }
    run(mapProp)

    val pInt = Gen.choose(Int.MinValue, Int.MaxValue).map(Par.unit)
    val forkProp =
      forAllPar(pInt)(x => equal(Par.fork(x), x))
    run(forkProp)
  }
}
