package sandbox.fpinscala.applicative

import sandbox.fpinscala.monad.Functor

trait Applicative[F[_]] extends Functor[F] { self =>
  // primitive combinators
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  // derived combinators
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
    f: (A, B, C, D) => E
  ): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def product[G[_]](G: Applicative[G]): Applicative[Lambda[x => (F[x], G[x])]] =
    new Applicative[Lambda[x => (F[x], G[x])]] {
      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(
        f: (A, B) => C
      ): (F[C], G[C]) = {
        val (f1, g1) = fa
        val (f2, g2) = fb
        (self.map2(f1, f2)(f), G.map2(g1, g2)(f))
      }

      override def unit[A](a: => A): (F[A], G[A]) =
        (self.unit(a), G.unit(a))
    }

  def compose[G[_]](G: Applicative[G]): Applicative[Lambda[x => F[G[x]]]] =
    new Applicative[Lambda[x => F[G[x]]]] {
      override def map2[A, B, C](fa: F[G[A]],
                                 fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)(G.map2(_, _)(f))

      override def unit[A](a: => A): F[G[A]] =
        self.unit(G.unit(a))
    }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft(unit(Map[K, V]())) {
      case (acc, (k, fv)) => map2(acc, fv)((m, v) => m + (k -> v))
    }

  // showing these can be implemented in terms of each other
  // either unit and apply or unit and map2 can be primitive
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2ViaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)
}

object Applicative {
  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E] = new Applicative[Validation[E, ?]] {
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(
      f: (A, B) => C
    ): Validation[E, C] =
      (fa, fb) match {
        case (Failure(head1, tail1), Failure(head2, tail2)) =>
          Failure(head1, tail1 ++ (head2 +: tail2))
        case (fail @ Failure(_, _), Success(_)) => fail
        case (Success(_), fail @ Failure(_, _)) => fail
        case (Success(a), Success(b))           => Success(f(a, b))
      }

    override def unit[A](a: => A): Validation[E, A] = Success(a)
  }
}
