package sandbox.functor

import cats._
import cats.implicits._

object Main extends App {
  // Tree exercise
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value)         => Leaf(f(value))
    }
  }

  println(Functor[Tree].map(Branch(Leaf("2"), Leaf("3")))(_.toInt + 69))
  println(Tree.leaf(4).map(_ - 2))
  println(Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2))

  // Contramap exercise
  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  implicit val stringPrintable: Printable[String] =
    (value: String) => "\"" + value + "\""

  implicit val booleanPrintable: Printable[Boolean] =
    (value: Boolean) => if (value) "yes" else "no"

  println(format("hello"))
  println(format(true))

  final case class Box[A](value: A)

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]](_.value)

  println(format(Box("testing")))
  println(format(Box(false)))
}
