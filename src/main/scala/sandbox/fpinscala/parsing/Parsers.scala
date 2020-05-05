package sandbox.fpinscala.parsing

import sandbox.fpinscala.propertytesting._
import Prop._

trait Parsers[ParseError, Parser[+ _]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def orString(s1: String, s2: String): Parser[String]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    @annotation.tailrec
    def loop(n: Int, acc: Parser[List[A]]): Parser[List[A]] =
      if (n <= 0) acc
      else loop(n - 1, map2(p, acc)(_ :: _))

    loop(n, succeed(Nil))
  }

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, p.many)(_ :: _) | succeed(Nil)

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, p.many)(_ :: _)

  def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)]

  def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
    (p ** p2).map(f.tupled)

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(
    implicit f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice: Parser[String] = self.slice(p)
    def **[B](pb: Parser[B]): Parser[(A, B)] = self.product(p, pb)
    def many1: Parser[List[A]] = self.many1(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def productAssociative[A, B, C](a: Parser[A], b: Parser[B], c: Parser[C])(
      in: Gen[String]
    ): Prop =
      equal((a ** b) ** c map unbiasL, a ** (b ** c) map unbiasR)(in)

    def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)
    def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)
  }
}
