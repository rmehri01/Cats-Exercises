package sandbox.casestudies.mapreduce

import cats.{Foldable, Monoid}
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Main extends App {
  def foldMap[A, B: Monoid](seq: Vector[A])(f: A => B): B =
    seq.map(f).combineAll

  println(foldMap(Vector(1, 2, 3))(identity))
  println(foldMap(Vector(1, 2, 3))(_.toString + "! "))
  println(foldMap("Hello world!".toVector)(_.toString.toUpperCase))

  def parallelFoldMap[A, B: Monoid](
    values: Vector[A]
  )(func: A => B): Future[B] = {
    val processors = Runtime.getRuntime.availableProcessors
    val groupSize = (values.size / processors).ceil.toInt

    val futures = values
      .grouped(groupSize)
      .map(vector => Future(foldMap(vector)(func)))

    Future
      .sequence(futures)
      .map(iter => iter.foldLeft(Monoid[B].empty)(_ |+| _))
  }

  val result: Future[Int] =
    parallelFoldMap((1 to 1000000).toVector)(identity)
  println(Await.result(result, 1.second))

  def parallelFoldMapCats[A, B: Monoid](
    values: Vector[A]
  )(func: A => B): Future[B] = {
    val processors = Runtime.getRuntime.availableProcessors
    val groupSize = (values.size / processors).ceil.toInt

    values
      .grouped(groupSize)
      .toVector
      .traverse(vector => Future(vector.foldMap(func)))
      .map(_.combineAll)
  }
  
}
