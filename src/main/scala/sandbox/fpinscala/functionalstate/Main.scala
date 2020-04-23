package sandbox.fpinscala.functionalstate

import RNG._

object Main {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(1)
    println(nonNegativeInt(rng))
    println(double(rng))
    println(ints(10)(rng))
    println(intsSequenced(10)(rng))
  }
}
