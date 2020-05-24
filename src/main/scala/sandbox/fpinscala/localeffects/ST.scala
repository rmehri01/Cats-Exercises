package sandbox.fpinscala.localeffects

sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)
  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S, A](a: => A) = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S) = (memo, s)
    }
  }
  def runST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1
}

sealed trait STRef[S, A] {
  protected var cell: A
  def read: ST[S, A] = ST(cell)
  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] =
    ST(new STRef[S, A] {
      var cell = a
    })
}

trait RunnableST[A] { def apply[S]: ST[S, A] }

sealed abstract class STArray[S, A] {
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.length)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  def fill(xs: Map[Int, A]): ST[S, Unit] =
    xs.foldRight(ST[S, Unit](())) {
      case ((i, v), st) => st flatMap (_ => write(i, v))
    }

  def swap(i: Int, j: Int): ST[S, Unit] =
    for {
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    } yield ()

}

object STArray {
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = Array.fill(sz)(v)
    })

  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value = xs.toArray
    })

  def noop[S]: ST[S, Unit] = ST[S, Unit](())

  def partition[S](arr: STArray[S, Int],
                   n: Int,
                   r: Int,
                   pivot: Int): ST[S, Int] =
    for {
      pivotVal <- arr.read(pivot)
      _ <- arr.swap(pivot, r)
      j <- STRef(n)
      _ <- (n until r).foldRight(noop[S]) { (i, s) =>
        for {
          _ <- s
          iVal <- arr.read(i)
          _ <- if (iVal < pivotVal) for {
            jVal <- j.read
            _ <- arr.swap(i, jVal)
            _ <- j.write(jVal + 1)
          } yield ()
          else noop[S]
        } yield ()
      }
      x <- j.read
      _ <- arr.swap(x, r)
    } yield x

  def qs[S](a: STArray[S, Int], n: Int, r: Int): ST[S, Unit] =
    if (n < r) for {
      pi <- partition[S](a, n, r, n + (n - r) / 2)
      _ <- qs(a, n, pi - 1)
      _ <- qs(a, pi + 1, r)
    } yield ()
    else noop[S]

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs
    else
      ST.runST(new RunnableST[List[Int]] {
        def apply[S]: ST[S, List[Int]] =
          for {
            arr <- STArray.fromList(xs)
            size <- arr.size
            _ <- qs(arr, 0, size - 1)
            sorted <- arr.freeze
          } yield sorted
      })

}

import scala.collection.mutable

sealed abstract class STMap[S, K, V] {
  protected def value: mutable.HashMap[K, V]

  def apply(k: K): ST[S, V] = ST(value(k))

  def size: ST[S, Int] = ST(value.size)

  def write(k: K, v: V): ST[S, Unit] = ST(value(k) = v)

  def +=(k: K, v: V): ST[S, Unit] = ST(value += k -> v)

  def -=(k: K): ST[S, Unit] = ST(value -= k)

  def freeze: ST[S, List[(K, V)]] = ST(value.toList)
}

object STMap {
  def empty[S, K, V]: ST[S, STMap[S, K, V]] =
    ST(new STMap[S, K, V] {
      override protected def value: mutable.HashMap[K, V] =
        mutable.HashMap.empty
    })
}
