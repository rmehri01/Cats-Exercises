package sandbox.fpinscala.propertytesting

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => apply(n).flatMap(f(_)(n)))

  def map[B](f: A => B): SGen[B] =
    SGen(n => apply(n).map(f))

  def **[B](s2: SGen[B]): SGen[(A,B)] =
    SGen(n => apply(n) ** s2(n))
}