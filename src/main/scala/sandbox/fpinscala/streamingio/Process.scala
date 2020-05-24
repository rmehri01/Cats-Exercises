package sandbox.fpinscala.streamingio

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
      await(i => emit(acc + 1, go(acc + 1)))

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

}
