package sandbox.fpinscala.applicative

import sandbox.fpinscala.monad.{Functor, Monad}

trait Traverse[F[_]] extends Functor[F] {
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
