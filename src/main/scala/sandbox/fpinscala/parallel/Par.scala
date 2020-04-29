package sandbox.fpinscala.parallel

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone = true
    override def get(timeout: Long, units: TimeUnit): A = get
    override def isCancelled = false
    override def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      MapToFuture(af, bf, f)
    }

  private case class MapToFuture[A, B, C](af: Future[A],
                                          bf: Future[B],
                                          f: (A, B) => C)
      extends Future[C] {

    @volatile var cache: Option[C] = None

    override def isDone: Boolean = cache.isDefined

    override def isCancelled: Boolean = af.isCancelled || bf.isCancelled

    override def cancel(mayInterruptIfRunning: Boolean): Boolean =
      af.cancel(mayInterruptIfRunning) || bf.cancel(mayInterruptIfRunning)

    override def get(): C = compute(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = af.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime
        val aTime = stop - start
        val br = bf.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call: A = a(es).get
      })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(Nil)) {
      case (pa, pas) => map2(pa, pas)(_ :: _)
    }

  // more efficient sequence
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequenceEfficient[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val ps = as.map(a => if (f(a)) List(unit(a)) else List())
    sequence(ps.flatten)
  }

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
//    choiceN(map(cond)(if (_) 1 else 0))(List(f, t))
    chooser(cond)(if (_) t else f)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
//    es => {
//      val i = run(es)(n).get
//      run(es)(choices(i))
//    }
    chooser(n)(choices(_))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
//    es => {
//      val k = run(es)(key).get
//      run(es)(choices(k))
//    }
    chooser(key)(choices(_))

  // same as flatMap or bind for sequencing computations
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val a = run(es)(pa).get
      run(es)(choices(a))
    }

  def flatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    join(map(pa)(choices))

  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  
}
