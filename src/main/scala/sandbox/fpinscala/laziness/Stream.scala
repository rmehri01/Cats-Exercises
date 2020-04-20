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
      case (Cons(h, t), num) if num > 0 => Some((h(), (t(), num - 1)))
      case _                            => None
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

//    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)
    unfold[A, Stream[A]](this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _                    => None
    }

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
    unfold[B, Stream[A]](this) {
      case Empty      => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](that: => Stream[B]): Stream[B] =
    foldRight(that)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold[C, (Stream[A], Stream[B])]((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _                            => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold[(Option[A], Option[B]), (Stream[A], Stream[B])]((this, s2)) {
      case (Empty, Empty)        => None
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), empty)))
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(((Some(h1()), Some(h2())), (t1(), t2())))
    }

  def startsWith[B >: A](s: Stream[B]): Boolean =
    this.zipAll(s).takeWhile(_._2.isDefined).forall {
      case (o1, o2) => o1 == o2
    }

  def tails: Stream[Stream[A]] =
    unfold[Stream[A], Stream[A]](this) {
      case Empty          => None
      case s @ Cons(_, t) => Some((s, t()))
    } append Stream(empty)

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
    this match {
      case Empty => Stream(z)
      case Cons(h, t) =>
        val tail = t().scanRight(z)(f)
        cons(f(h(), tail.headOption.getOrElse(z)), tail)
    }
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
