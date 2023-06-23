package Chapter4

import Chapter4.FOption.{FNone, FSome}

enum FOption[+A] {
  case FSome(get: A)
  case FNone

  def map[B](f: A => B): FOption[B] = this match
    case FSome(value) => FSome(f(value))
    case _ => FNone

  def flatMap[B](f: A => FOption[B]): FOption[B] = this.map(f(_)).getOrElse(FNone)

  def getOrElse[B >: A](default: => B): B = this match
    case FSome(value) => value
    case _ => default

  def orElse[B >: A](ob: => FOption[B]): FOption[B] = this match
    case FNone => ob
    case _ => this

  def filter(f: A => Boolean): FOption[A] = this.flatMap(v => if f(v) then this else FNone)
}

object FOption {
  def apply[A](value: A): FOption[A] = FSome(value)

  def nonEmpty[A](l: Seq[A]): FOption[Seq[A]] =
    if l.isEmpty then FNone
    else FSome(l)

  def mean(values: Seq[Double]): FOption[Double] =
    nonEmpty(values).map(_.reduce(_ + _)).map(_ / values.length)

  def variance(values: Seq[Double]): FOption[Double] = for {
    average <- mean(values)
    squaredDeviations = values.map(value => math.pow(value - average, 2))
  } yield squaredDeviations.sum / values.length

  def map2[A, B, C](oa: FOption[A], ob: FOption[B])(f: (A, B) => C): FOption[C] = for
      a <- oa
      b <- ob
    yield f(a, b)

  def sequence[A](as: List[FOption[A]]): FOption[List[A]] =
    as.foldRight(FSome(List.empty[A]))(
      (oa, optionList) => oa.flatMap(a => optionList.map(l => a :: l))
    )

}


