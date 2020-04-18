package sandbox.fpinscala

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    println((0 to 10).map(fib))
    println(isSorted[Int](Array(1, 2, 3), _ < _))
  }

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, current: Int): Int =
      if (n == 0) prev
      else loop(n - 1, current, prev + current)

    loop(n, 0, 1)
  }

  @tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    if (as.length < 2) true
    else ordered(as(0), as(1)) && isSorted(as.tail, ordered)

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
