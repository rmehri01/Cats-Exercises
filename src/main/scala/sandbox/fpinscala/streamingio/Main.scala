package sandbox.fpinscala.streamingio

import Process._

object Main {
  def main(args: Array[String]): Unit = {
    val s = mean(Stream(1.0, 2.0, 3.0, 4.0)).toList
    println(s)
  }
}
