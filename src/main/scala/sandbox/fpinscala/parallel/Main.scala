package sandbox.fpinscala.parallel

import java.util.concurrent.{Executors, TimeUnit}

import sandbox.fpinscala.parallel.Par._

object Main {
  def main(args: Array[String]): Unit = {
    val seq = IndexedSeq(1, 2, 3)
    val es = Executors.newFixedThreadPool(8)
    println(run(es)(sum(seq)).get(1, TimeUnit.SECONDS))
    println(run(es)(parFilter(seq.toList)(_ % 2 == 1)).get(1, TimeUnit.SECONDS))
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

}
