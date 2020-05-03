package sandbox.fpinscala.propertytesting

import java.util.concurrent.{ExecutorService, Executors}

import Prop._
import sandbox.fpinscala.functionalstate.{RNG, SimpleRNG}
import sandbox.fpinscala.laziness.Stream
import sandbox.fpinscala.parallel.Par
import sandbox.fpinscala.parallel.Par._

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def tag(msg: String): Prop = Prop { (maxSize, n, rng) =>
    run(maxSize, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x               => x
    }
  }

  def &&(p: Prop): Prop = Prop { (maxSize, n, rng) =>
    run(maxSize, n, rng) match {
      case Passed | Proved => p.run(maxSize, n, rng)
      case x               => x
    }
  }

  def ||(p: Prop): Prop = Prop { (maxSize, n, rng) =>
    run(maxSize, n, rng) match {
      case Falsified(failure, _) => p.tag(failure).run(maxSize, n, rng)
      case x                     => x
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
  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

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

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p, p2)(_ == _)

  val S: Gen[ExecutorService] = Gen.weighted(
    Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25
  )

  import Gen.**

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

}
