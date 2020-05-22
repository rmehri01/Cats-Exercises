package sandbox.fpinscala.applicative

import sandbox.fpinscala.functionalstate.State
import sandbox.fpinscala.monad.{Functor, Monad}
import sandbox.fpinscala.monoid.{Foldable, Monoid}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverse[G[_], A, B](
    fa: F[A]
  )(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(identity)

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[Const[M, ?]] {
      def unit[A](a: => A): M = M.zero
      def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
    }

  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[Const[M, ?], A, Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[State[S, ?], A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)(
      (a: A) =>
        for {
          s1 <- State.get[S]
          (b, s2) = f(a, s1)
          _ <- State.set(s2)
        } yield b
    ).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, s) => (s.head, s.tail))._1

  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(as, z)((a, s) => ((), f(s, a)))._2

  def fuse[G[_], H[_], A, B](fa: F[A])(
    f: A => G[B],
    g: A => H[B]
  )(G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[Lambda[x => (G[x], H[x])], A, B](fa)(a => (f(a), g(a)))(
      G product H
    )

  def compose[H[_]](implicit H: Traverse[H]): Traverse[Lambda[x => F[H[x]]]] =
    new Traverse[Lambda[x => F[H[x]]]] {
      override def traverse[G[_], A, B](
        fa: F[H[A]]
      )(f: A => G[B])(implicit G: Applicative[G]): G[F[H[B]]] =
        self.traverse(fa)(ha => H.traverse(ha)(f))
    }
}

object Traverse {
  val listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[G[_], A, B](
      fa: List[A]
    )(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]())) {
        case (a, glb) => G.map2(f(a), glb)(_ :: _)
      }
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_], A, B](
      fa: Option[A]
    )(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa match {
        case Some(value) => G.map(f(value))(Some(_))
        case None        => G.unit(None)
      }
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[G[_], A, B](
      fa: Tree[A]
    )(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(
        f(fa.head),
        listTraverse.traverse(fa.tail)(tree => traverse(tree)(f))
      )(Tree(_, _))

  }
}
