package sandbox.fpinscala.io

import sandbox.fpinscala.monad.Monad

sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] =
    flatMap(f andThen (Return(_)))
}

case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B])
    extends Free[F, B]

object Free {
  def freeMonad[F[_]]: Monad[Free[F, ?]] =
    new Monad[Free[F, ?]] {
      override def unit[A](a: => A): Free[F, A] = Return(a)

      override def flatMap[A, B](
        ma: Free[F, A]
      )(f: A => Free[F, B]): Free[F, B] =
        ma flatMap f
    }

  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a)  => a
    case Suspend(s) => s()
    case FlatMap(s, f) =>
      s match {
        case Return(a)     => runTrampoline(f(a))
        case Suspend(s)    => runTrampoline(f(s()))
        case FlatMap(s, g) => runTrampoline(s flatMap (a => g(a) flatMap f))
      }
  }

  @annotation.tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f)     => step(f(x))
    case _                         => a
  }

  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a)  => F.unit(a)
    case Suspend(r) => r
    case FlatMap(x, f) =>
      x match {
        case Suspend(r) => F.flatMap(r)(a => run(f(a)))
        case _          => sys.error("Impossible; `step` eliminates these cases")
      }
  }

}
