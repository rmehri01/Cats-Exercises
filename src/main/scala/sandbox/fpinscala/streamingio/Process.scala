package sandbox.fpinscala.streamingio

import Process._
import sandbox.fpinscala.monad.Monad

sealed trait Process[I, O] {
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) =>
      s match {
        case h #:: t => recv(Some(h))(t)
        case xs      => recv(None)(xs)
      }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) =>
        Await {
          case None => recv(None)
          case i    => go(recv(i))
        }
      case Emit(h, t) => Emit(h, go(t))
    }

    go(this)
  }

  def |>[O2](p2: Process[O, O2]): Process[I, O2] =
    p2 match {
      case Halt()           => Halt()
      case Emit(head, tail) => Emit(head, this |> tail)
      case Await(f) =>
        this match {
          case Halt()           => Halt() |> f(None)
          case Emit(head, tail) => tail |> f(Some(head))
          case Await(g)         => Await(i => g(i) |> p2)
        }
    }

  def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

  def ++(p: => Process[I, O]): Process[I, O] = this match {
    case Halt()      => p
    case Emit(h, t)  => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt()      => Halt()
    case Emit(h, t)  => f(h) ++ t.flatMap(f)
    case Await(recv) => Await(recv andThen (_ flatMap f))
  }

  def zipWithIndex: Process[I, (O, Int)] =
    zip(this, count map (_ - 1))

}

case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]())
    extends Process[I, O]
case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
case class Halt[I, O]() extends Process[I, O]

object Process {
  def await[I, O](f: I => Process[I, O],
                  fallback: Process[I, O] = Halt[I, O]()): Process[I, O] =
    Await[I, O] {
      case Some(i) => f(i)
      case None    => fallback
    }

  def emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()): Process[I, O] =
    Emit(head, tail)

  def id[I]: Process[I, I] = lift(identity)

  def liftOne[I, O](f: I => O): Process[I, O] = Await {
    case Some(i) => Emit(f(i))
    case None    => Halt()
  }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(i) if p(i) => Emit(i)
      case _               => Halt()
    }.repeat

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] =
      Await {
        case Some(d) => Emit(d + acc, go(d + acc))
        case None    => Halt()
      }

    go(0.0)
  }

  def take[I](n: Int): Process[I, I] =
    if (n <= 0) Halt()
    else
      await(i => emit(i, take(n - 1)))

  def drop[I](n: Int): Process[I, I] =
    if (n <= 0) id
    else await(_ => drop(n - 1))

  def takeWhile[I](f: I => Boolean): Process[I, I] =
    await(
      i =>
        if (f(i)) emit(i, takeWhile(f))
        else Halt()
    )

  def dropWhile[I](f: I => Boolean): Process[I, I] =
    await(
      i =>
        if (f(i)) dropWhile(f)
        else emit(i, id)
    )

  def count[I]: Process[I, Int] = {
    def go(acc: Int): Process[I, Int] =
      await(_ => emit(acc + 1, go(acc + 1)))

    go(0)
  }

  def mean: Process[Double, Double] = {
    def go(sum: Double, n: Double): Process[Double, Double] = {
      await(i => emit((i + sum) / (n + 1), go(i + sum, n + 1)))
    }

    go(0, 0)
  }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
    await(
      i =>
        f(i, z) match {
          case (o, s2) => emit(o, loop(s2)(f))
      }
    )

  def sumViaLoop: Process[Double, Double] =
    loop(0.0)((i, s) => (i + s, i + s))

  def countViaLoop[I]: Process[I, Int] =
    loop(0)((_, s) => (s + 1, s + 1))

  def monad[I]: Monad[Process[I, ?]] =
    new Monad[Process[I, ?]] {
      def unit[O](o: => O): Process[I, O] = Emit(o)

      def flatMap[O, O2](
        p: Process[I, O]
      )(f: O => Process[I, O2]): Process[I, O2] =
        p flatMap f
    }

  def meanViaZip: Process[Double, Double] =
    zip[Double, Double, Int](sum, count).map { case (s, n) => s / n }

  def zip[A, B, C](p1: Process[A, B], p2: Process[A, C]): Process[A, (B, C)] =
    (p1, p2) match {
      case (Halt(), _) | (_, Halt())  => Halt()
      case (Emit(b, t1), Emit(c, t2)) => Emit((b, c), zip(t1, t2))
      case (Await(recv1), _) =>
        Await((oa: Option[A]) => zip(recv1(oa), feed(oa)(p2)))
      case (_, Await(recv2)) =>
        Await((oa: Option[A]) => zip(feed(oa)(p1), recv2(oa)))
    }

  def feed[A, B](oa: Option[A])(p: Process[A, B]): Process[A, B] =
    p match {
      case Halt()      => p
      case Emit(h, t)  => Emit(h, feed(oa)(t))
      case Await(recv) => recv(oa)
    }

  def exists[I](f: I => Boolean): Process[I, Boolean] =
    lift(f) |> any

  def any: Process[Boolean, Boolean] =
    loop(false)((i, s) => (i || s, i || s))

  def convertFahrenheit: Process[String,String] =
    filter((line: String) => !line.startsWith("#")) |>
      filter(line => line.trim.nonEmpty) |>
      lift(line => toCelsius(line.toDouble).toString)

  def toCelsius(fahrenheit: Double): Double =
    (5.0 / 9.0) * (fahrenheit - 32.0)
}
