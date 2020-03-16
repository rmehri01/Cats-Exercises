package sandbox.monad

object Main extends App {
  type Id[A] = A

  def pure[A](a: A): Id[A] = a

  def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)

  def map[A, B](value: Id[A])(func: A => B): Id[B] = func(value)

  println(pure(2))
  println(flatMap("foo")(_ + " bar"))
}
