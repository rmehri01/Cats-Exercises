package sandbox.fpinscala.propertytesting

import Prop._
import sandbox.fpinscala.functionalstate.RNG

import sandbox.fpinscala.laziness.Stream

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props
        .map(
          p =>
            Prop { (max, _, rng) =>
              p.run(max, casesPerSize, rng)
          }
        )
        .toList
        .reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (_, n, rng) =>
    randomStream(as)(rng)
      .zip(Stream.from(0))
      .take(n)
      .map {
        case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def tag(msg: String): Prop = Prop { (maxSize, n, rng) =>
    run(maxSize, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x               => x
    }
  }

  def &&(p: Prop): Prop = Prop { (maxSize, n, rng) =>
    run(maxSize, n, rng) match {
      case Passed => p.run(maxSize, n, rng)
      case x      => x
    }
  }

  def ||(p: Prop): Prop = Prop { (maxSize, n, rng) =>
    run(maxSize, n, rng) match {
      case Falsified(failure, _) => p.tag(failure).run(maxSize, n, rng)
      case Passed                => Passed
    }
  }
}

object Prop {
  type MaxSize = Int
  type FailedCase = String
  type TestCases = Int
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount)
      extends Result {
    override def isFalsified = true
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}
