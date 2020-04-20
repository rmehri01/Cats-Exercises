package sandbox.fpinscala.errorhandling

object Main {
  def main(args: Array[String]): Unit = {
    println(map2(Some(2), None: Option[Int])(_ + _))
    println(sequence(List(Some(2), None)))
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    for {
      m <- mean(xs)
      diffSquared = xs.map(x => math.pow(x - m, 2))
      variance <- mean(diffSquared)
    } yield variance

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- a
      b <- b
    } yield f(a, b)

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((a, acc) => map2(f(a), acc)(_ :: _))

  def sequenceEither[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverseEither(es)(identity)

  def traverseEither[E, A, B](
    as: List[A]
  )(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))(
      (a, acc) => for { aa <- f(a); acc <- acc } yield aa :: acc
    )
}
