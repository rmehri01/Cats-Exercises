package sandbox.fpinscala.parsing

import sandbox.fpinscala.parsing.Parser.Parser

import scala.util.matching.Regex

object Parser extends Parsers[Parser] {
  type Parser[+A] = String => Either[ParseError, A]

  override def slice[A](p: Parser[A]): Parser[String] = ???

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  override def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def attempt[A](p: Parser[A]): Parser[A] = ???

  override implicit def string(s: String): Parser[String] =
    input =>
      if (input.startsWith(s)) Right(s)
      else Left(Location(input).toError("Expected: " + s))

  override implicit def regex(r: Regex): Parser[String] = ???
}
