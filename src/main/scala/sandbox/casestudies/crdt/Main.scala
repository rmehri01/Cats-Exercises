package sandbox.casestudies.crdt

import cats.implicits._

object Main extends App {
  val gCounterInt1 = GCounter(Map("a" -> 2, "b" -> 5))
  val gCounterInt2 = GCounter(Map("a" -> 5, "b" -> 3))
  println(gCounterInt1.total)
  println((gCounterInt1 merge gCounterInt2).counters)
  println(gCounterInt1.increment("a", 5).counters)

  val gCounterSet1 = GCounter(Map("a" -> Set(2), "b" -> Set(1, 5)))
  val gCounterSet2 = GCounter(Map("a" -> Set(5), "b" -> Set(3)))
  println(gCounterSet1.total)
  println((gCounterSet1 merge gCounterSet2).counters)
  println(gCounterSet1.increment("a", Set(20)).counters)
}
