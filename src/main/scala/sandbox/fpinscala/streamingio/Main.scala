package sandbox.fpinscala.streamingio

import Process._

object Main {
  def main(args: Array[String]): Unit = {
    val stream = Stream(1.0, 2.0, 3.0, 4.0)
    println(mean.zipWithIndex(stream).toList)
    println(exists((_: Double) % 3 == 0)(stream).toList)
  }
}
