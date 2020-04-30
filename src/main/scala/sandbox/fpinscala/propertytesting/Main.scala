package sandbox.fpinscala.propertytesting

import sandbox.fpinscala.functionalstate.{RNG, SimpleRNG}

object Main {
  def main(args: Array[String]): Unit = {
    val rng: RNG = SimpleRNG(1)
    val gen: Gen[Int] = Gen.unit(33)
    println(Gen.choose(0, 2).sample.run(rng))
    println(Gen.listOfN(5, gen).sample.run(rng))
  }
}
