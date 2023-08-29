package Chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

opaque type Par[A] = ExecutorService => Future[A]

object Par {
  def unit[A](a: => A): Par[A] = es => UnitFuture(a)
  def fork[A](pa: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call: A =
        pa(es).get
    })
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  extension [A](pa: Par[A]) def run(executorService: ExecutorService): Future[A] = pa(executorService)

  extension [A](pa: Par[A]) def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) =>
      val futureA = pa(es)
      val futureB = pb(es)
      UnitFuture(f(futureA.get, futureB.get))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  extension[A] (pa: Par[A]) def map[B](f: A => B): Par[B] =
    pa.map2(unit(()))((a, _) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A]))((pa, acc) => pa.map2(acc)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    fork:
      val pars: List[Par[Option[A]]] = as.map(asyncF(a => Some(a).filter(f)))
      sequence(pars).map(_.flatten)


  private case class UnitFuture[A](get: A) extends Future[A]:
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isCancelled: Boolean = false
    override def isDone: Boolean = true
    override def get(timeout: Long, unit: TimeUnit): A = get
}


