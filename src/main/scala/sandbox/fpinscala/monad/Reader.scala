package sandbox.fpinscala.monad

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R]: Monad[Reader[R, ?]] = new Monad[Reader[R, ?]] {
    def unit[A](a: => A): Reader[R,A] = Reader(_ => a)

    def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
      Reader(r => f(st.run(r)).run(r))
  }
}
