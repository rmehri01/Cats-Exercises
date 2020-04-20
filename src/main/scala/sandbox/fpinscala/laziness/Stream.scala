package sandbox.fpinscala.laziness

import Stream._

sealed trait Stream[+A] {
  def toList: List[A] =
    this match {
      case Empty      => Nil
      case Cons(h, t) => h() :: t().toList
    }

  def take(n: Int): Stream[A] =
//    this match {
//      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
//      case _                   => empty
//    }
    unfold[A, (Stream[A], Int)]((this, n)) {
      case (s, num) =>
        if (num <= 0) None
        else s.headOption.map(a => (a, (s.drop(1), num - 1)))
    }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _                   => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
//    this match {
//      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
//      case _                    => empty
//    }
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forall(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
//    foldRight(empty[B])((a, b) => cons(f(a), b))
    unfold[B, Stream[A]](this)(s => s.headOption.map(a => (f(a), s.drop(1))))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](that: => Stream[B]): Stream[B] =
    foldRight(that)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] =
//    cons(a, constant(a))
    unfold(a)(_ => Some((a, a)))

  def from(n: Int): Stream[Int] =
//    cons(n, from(n + 1))
    unfold(n)(s => Some((s, s + 1)))

  def fibs: Stream[Int] = {
//    def loop(n1: Int, n2: Int): Stream[Int] =
//      cons(n1, loop(n2, n1 + n2))
//
//    loop(0, 1)

    unfold((0, 1)) { case (n1, n2) => Some((n1, (n2, n1 + n2))) }
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z)
      .map { case (a, s) => cons(a, unfold(s)(f)) }
      .getOrElse(empty)

}
