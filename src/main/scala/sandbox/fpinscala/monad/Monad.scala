package sandbox.fpinscala.monad

import sandbox.fpinscala.applicative.{Applicative, Traverse}
import sandbox.fpinscala.functionalstate.State
import sandbox.fpinscala.parallel.Par.Par
import sandbox.fpinscala.parallel.Par
import sandbox.fpinscala.parsing.Parsers
import sandbox.fpinscala.propertytesting.Gen

trait Monad[F[_]] extends Applicative[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  override def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case a :: as =>
        flatMap(f(a))(bool => {
          val r = filterM(as)(f)
          if (!bool) r else map(r)(a :: _)
        })
    }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(identity)

  def flatMapViaJoinAndMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def composeM[G[_], H[_]](implicit G: Monad[G],
                           H: Monad[H],
                           T: Traverse[H]): Monad[Lambda[x => G[H[x]]]] =
    new Monad[Lambda[x => G[H[x]]]] {
      override def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))

      override def flatMap[A, B](ma: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
        G.flatMap(ma)(ga => G.map(T.traverse(ga)(f))(H.join))
    }

}

object Monad {
  val genMonad: Monad[Gen] = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  def parserMonad[P[+ _]](p: Parsers[P]): Monad[P] = new Monad[P] {
    override def unit[A](a: => A): P[A] = p.succeed(a)

    override def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] =
      p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma.flatMap(f)
  }

  def stateMonad[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {
    override def unit[A](a: => A): State[S, A] = State.unit(a)

    override def flatMap[A, B](
      ma: State[S, A]
    )(f: A => State[S, B]): State[S, B] =
      ma.flatMap(f)
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] =
      ma flatMap f
  }

  def eitherMonad[E]: Monad[Either[E, ?]] = new Monad[Either[E, ?]] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](
      ma: Either[E, A]
    )(f: A => Either[E, B]): Either[E, B] =
      ma flatMap f
  }
}
