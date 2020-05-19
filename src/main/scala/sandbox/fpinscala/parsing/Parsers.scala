package sandbox.fpinscala.parsing

import sandbox.fpinscala.propertytesting._
import Prop._

import scala.util.matching.Regex

trait Parsers[Parser[+ _]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    @annotation.tailrec
    def loop(n: Int, acc: Parser[List[A]]): Parser[List[A]] =
      if (n <= 0) acc
      else loop(n - 1, map2(p, acc)(_ :: _))

    loop(n, succeed(Nil))
  }

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, p.many)(_ :: _) | succeed(Nil)

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, p.many)(_ :: _)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for { a <- p; b <- p2 } yield (a, b)

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
//    (p ** p2).map(f.tupled)
    for { a <- p; b <- p2 } yield f(a, b)

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def skipL[A, B](pa: Parser[A], pb: => Parser[B]): Parser[B] =
    map2(pa.slice, pb)((_, b) => b)

  def skipR[A, B](pa: Parser[A], pb: => Parser[B]): Parser[A] =
    map2(pa, pb.slice)((a, _) => a)

  def sepBy[A, B](element: Parser[A], sep: Parser[B]): Parser[List[A]] =
    map2(element, (sep *> element).many)(_ :: _) | succeed(Nil)

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  import scala.language.implicitConversions

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(
    implicit f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice: Parser[String] = self.slice(p)
    def **[B](pb: => Parser[B]): Parser[(A, B)] = self.product(p, pb)
    def many1: Parser[List[A]] = self.many1(p)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def *>[B](pb: => Parser[B]): Parser[B] = self.skipL(p, pb)
    def <*[B](pb: => Parser[B]): Parser[A] = self.skipR(p, pb)
    def sepBy[B](sep: Parser[B]): Parser[List[A]] = self.sepBy(p, sep)
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

case class ParseError(stack: List[(Location, String)])

case class Location(input: String, offset: Int = 0) {
  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1        => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))
}
