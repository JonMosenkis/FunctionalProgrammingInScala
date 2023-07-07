package Chapter5

import Chapter5.FLazyList.{cons, empty, unfold}

import scala.annotation.tailrec

enum FLazyList[+A] {
  case Empty
  case Cons(head: () => A, tail: () => FLazyList[A])

  def toList: List[A] =
    @tailrec
    def loop(ll: FLazyList[A], acc: List[A]): List[A] = ll match
      case Empty => acc.reverse
      case Cons(head, tail) => loop(tail(), head() :: acc)

    loop(this, List.empty[A])

  def take(n: Int): FLazyList[A] =
    unfold((this, n)) ((a, s) => a match
      case Cons(head, tail) if s > 1 => Some(head(), (tail(), s - 1))
      case Cons(head, tail) if s == 1 => Some(head(), (empty, 0))
      case _ => None
    )

  def drop(n: Int): FLazyList[A] = this match
    case Cons(head, tail) if n > 0 => tail().drop(n-1)
    case _ => this

  def takeWhile(predicate: A => Boolean): FLazyList[A] =
    unfold(this) {
      case Cons(h, t) if predicate(h()) => Some(h(), t())
      case _ => None
    }

  def zipAll[B](that: FLazyList[B]): FLazyList[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
      case (Cons(ha, ta), Empty) => Some((Some(ha()), None), (ta(), empty))
      case (Empty, Cons(hb, tb)) => Some((None, Some(hb())), (empty, tb()))
      case _ => None
    }

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match
    case Cons(h, t) => f(h(), t().foldRight(acc)(f))
    case _ => acc

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption(): Option[A] = foldRight(Option.empty)((a, _) => Some(a))

  def filter(predicate: A => Boolean): FLazyList[A] =
    foldRight(empty[A])((a, acc) => if predicate(a) then cons(a, acc) else acc)

  def map[B](f: A => B): FLazyList[B] = unfold(this) {
    case Cons(head, tail) => Some((f(head()), tail()))
    case Empty => None
  }

  def flatMap[B](f: A => FLazyList[B]): FLazyList[B] =
    foldRight(empty[B])((a, acc) => f(a).append(acc))

  def append[B >: A](that: => FLazyList[B]): FLazyList[B] =
    foldRight(that)((a, acc) => cons(a, acc))

  def startsWith[B >: A](prefix: FLazyList[B]): Boolean =
    this.zipAll(prefix).takeWhile {
      case (_, Some(_)) => true
      case _ => false
    } .forAll {
      case (Some(a), Some(b)) => a == b
      case _ => false
    }

  def tails(): FLazyList[FLazyList[A]] =
    unfold(this) {
      case Cons(head, tail) => Some(Cons(head, tail), tail())
      case _ => None
    }.append(FLazyList(empty))

}

object FLazyList {
  def cons[A](head: => A, tail: => FLazyList[A]): FLazyList[A] =
    lazy val hd = head
    lazy val tl = tail
    Cons(() => hd, () => tl)

  def empty[A]: FLazyList[A] = Empty

  def apply[A](as: A*): FLazyList[A] =
    if as.isEmpty then empty[A]
    else cons(as.head, apply(as.tail*))

  def continually[A](a: A): FLazyList[A] = unfold(a)(_ => Some((a, a)))

  def from(n: Int): FLazyList[Int] = unfold(n)(l => Some((l, l + 1)))

  def fibs(): FLazyList[Int] =
    def internal(prev: Int, prePrev: Int): FLazyList[Int] =
      val cur = prev + prePrev
      cons(cur, internal(cur, prev))

    FLazyList(0, 1).append(internal(1, 0))

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): FLazyList[A] =
    f(state).map {
      case (a, s) => cons(a, unfold(s)(f))
    }.getOrElse(empty[A])

}
