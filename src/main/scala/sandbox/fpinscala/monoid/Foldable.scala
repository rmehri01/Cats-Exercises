package sandbox.fpinscala.monoid

import Main._
import sandbox.fpinscala.datastructures.{Branch, Leaf}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] =
    foldRight(fa)(Nil: List[A])(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
}

import sandbox.fpinscala.datastructures.Tree

object TreeFoldable extends Foldable[Tree] {
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Leaf(value)         => f(value, z)
      case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
    }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Leaf(value)         => f(z, value)
      case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
    }

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    Tree.fold(as)(f)(mb.op)
}

object OptionFoldable extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Some(value) => f(value, z)
      case None        => z
    }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Some(value) => f(z, value)
      case None        => z
    }

  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Some(value) => f(value)
      case None        => mb.zero
    }
}
