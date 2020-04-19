package sandbox.fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int =
//    tree match {
//      case Leaf(_)             => 1
//      case Branch(left, right) => 1 + size(left) + size(right)
//    }
    fold(tree)(_ => 1)(1 + _ + _)

  def maximum(t: Tree[Int]): Int =
//    t match {
//      case Leaf(value)         => value
//      case Branch(left, right) => maximum(left).max(maximum(right))
//    }
    fold(t)(x => x)(_ max _)

  def depth[A](t: Tree[A]): Int =
//    t match {
//      case Leaf(_)             => 0
//      case Branch(left, right) => 1 + (depth(left).max(depth(right)))
//    }
    fold(t)(_ => 0)((d1, d2) => 1 + d1 max d2)

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
//    t match {
//      case Leaf(value)         => Leaf(f(value))
//      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
//    }
    fold(t)(a => Leaf(f(a)): Tree[B])((b1, b2) => Branch(b1, b2))

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(value)         => f(value)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }
}
