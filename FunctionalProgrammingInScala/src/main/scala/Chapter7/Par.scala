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
      val result = UnitFuture(f(futureA.get, futureB.get))
      result


  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  extension[A] (pa: Par[A]) def map[B](f: A => B): Par[B] =
    pa.map2(unit(()))((a, _) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    sequenceBalanced(ps.toIndexedSeq).map(_.toList)

  def sequenceBalanced[A](pas: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    if pas.isEmpty then unit(IndexedSeq.empty)
    else if pas.size == 1 then pas.head.map(a => IndexedSeq(a))
    else
      val (l, r) = pas.splitAt(pas.size / 2)
      sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    fork:
      val pars: List[Par[Option[A]]] = as.map(asyncF(a => Some(a).filter(f)))
      sequence(pars).map(_.flatten)

  def countWords(paragraphs: List[String]): Par[Int] =
    def wordsInParagraph(p: String): Int =
      p.split(' ').length

    val parWords: List[Par[Int]] = paragraphs.map(asyncF(wordsInParagraph))
    sequence(parWords).map(_.sum)

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] =
    pa.map2(pb)((a, b) => (a, b)).map2(pc)((ab, c) => f(ab._1, ab._2, c))

  def choiceN[A](np: Par[Int])(choices: List[Par[A]]): Par[A] = es =>
    val n = np(es).get()
    choices(n)(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(cond.map(c => if c then 1 else 0))(List(t, f))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es =>
    val k = key(es).get()
    choices(k)(es)

  extension [A](pa: Par[A]) def flatMap[B](f: A => Par[B]): Par[B] = es =>
    val a = pa(es).get()
    f(a)(es)

  def join[A](ppa: Par[Par[A]]): Par[A] = es =>
    ppa(es).get()(es)

  private case class UnitFuture[A](get: A) extends Future[A]:
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isCancelled: Boolean = false
    override def isDone: Boolean = true
    override def get(timeout: Long, unit: TimeUnit): A = get
}


