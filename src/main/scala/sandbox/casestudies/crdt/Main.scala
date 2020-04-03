package sandbox.casestudies.crdt

import cats.implicits._

object Main extends App {
  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)
  val counter = GCounter[Map, String, Int]
  val merged = counter.merge(g1, g2)
  val total = counter.total(merged)

  println(merged)
  println(total)
}
