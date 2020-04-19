package sandbox.fpinscala.datastructures

import scala.annotation.tailrec

object Main {
  def tail[A](list: List[A]): List[A] =
    list match {
      case Nil     => sys.error("can't take tail of empty list")
      case _ :: xs => xs
    }

  def setHead[A](list: List[A], value: A): List[A] =
    list match {
      case Nil     => sys.error("can't set head of empty list")
      case _ :: xs => value :: xs
    }

  @tailrec
  def drop[A](list: List[A], n: Int): List[A] =
    if (n == 0) list
    else
      list match {
        case Nil     => Nil
        case _ :: xs => drop(xs, n - 1)
      }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil     => Nil
      case x :: xs => if (f(x)) dropWhile(xs, f) else l
    }

  def init[A](l: List[A]): List[A] =
    l.reverse.tail.reverse

  def length[A](as: List[A]): Int =
    as.foldRight(0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil     => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, x) => x :: acc)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)((a, b) => a :: b)

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    @tailrec
    def loop(as: List[A], acc: List[B]): List[B] = as match {
      case Nil     => reverse(acc)
      case x :: xs => loop(xs, f(x) :: acc)
    }

    loop(as, Nil)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    @tailrec
    def loop(as: List[A], acc: List[A]): List[A] = as match {
      case Nil     => reverse(acc)
      case x :: xs => loop(xs, if (f(x)) x :: acc else acc)
    }

    loop(as, Nil)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterWithFlatmap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addLists(l: List[Int], r: List[Int]): List[Int] =
    (l, r) match {
      case (Nil, _)           => Nil
      case (_, Nil)           => Nil
      case (x :: xs, y :: ys) => (x + y) :: addLists(xs, ys)
    }

  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] =
    (l, r) match {
      case (Nil, _)           => Nil
      case (_, Nil)           => Nil
      case (x :: xs, y :: ys) => f(x, y) :: zipWith(xs, ys)(f)
    }

  def startsWith[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (Nil, _)           => false
      case (_, Nil)           => true
      case (x :: xs, y :: ys) => x == y && startsWith(xs, ys)
    }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil                       => sub == Nil
      case _ if startsWith(sup, sub) => true
      case _ :: xs                   => hasSubsequence(xs, sub)
    }

  def main(args: Array[String]): Unit = {
    println(drop(List(1, 2, 3), 5))
    println(init(List(1, 2, 3)))
    println(reverse(List(1, 2, 3)))
    println(foldRight(List(1, 2, 3, 4), 0)(_ - _))
    println(foldLeft(List(1, 2, 3, 4), 0)(_ - _))
    println(append(List(1, 2, 3), List(4, 5, 6)))
    println(concat(List(List(1, 2), List(3, 4), List(5, 6))))
    println(filter(List(1, 2, 3))(_ > 1))
    println(flatMap(List(1, 2, 3))(i => List(i, i)))
    println(addLists(List(1, 2, 3), List(4, 5, 6, 7)))
    println(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
  }
}
