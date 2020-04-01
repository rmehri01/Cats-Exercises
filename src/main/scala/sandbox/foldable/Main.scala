package sandbox.foldable

import cats.Monoid
import cats.implicits._

object Main extends App {
  val list = List(1, 2, 3, 4, 5)
  println(list.foldLeft(List.empty[Int]) { case (acc, item)  => item :: acc })
  println(list.foldRight(List.empty[Int]) { case (item, acc) => item :: acc })

  // implementing list methods based on foldRight
  def map[A, B](list: List[A])(f: A => B): List[B] =
    list.foldRight(List.empty[B])((item, acc) => f(item) :: acc)

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
    list.foldRight(List.empty[B])((item, acc) => f(item) ::: acc)

  def filter[A](list: List[A])(f: A => Boolean): List[A] =
    list.foldRight(List.empty[A])(
      (item, acc) => if (f(item)) item :: acc else acc
    )

  def sum[A](list: List[A])(implicit monoid: Monoid[A]): A =
    list.foldRight(monoid.empty)(_ |+| _)

  println(map(list)(_ + 50))
  println(flatMap(list)(x => List(x, x)))
  println(filter(list)(_ % 2 == 0))
  println(sum(list))
}
