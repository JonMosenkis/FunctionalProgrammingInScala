package Chapter5

import Chapter5.FLazyList.empty

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

  def take(n: Int): FLazyList[A] = this match
    case Cons(head, tail) if n > 1 => FLazyList.cons(head(), tail().take(n-1))
    case Cons(head, tail) if n == 1 => FLazyList.cons(head(), empty)
    case _ => empty

  def drop(n: Int): FLazyList[A] = this match
    case Cons(head, tail) if n > 0 => tail().drop(n-1)
    case _ => this

  def takeWhile(predicate: A => Boolean): FLazyList[A] = this match
    case Cons(head, tail) =>
      val hh: A = head()
      if predicate(hh) then
        FLazyList.cons(hh, tail().takeWhile(predicate))
      else empty
    case _ => empty

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match
    case Cons(h, t) => f(h(), t().foldRight(acc)(f))
    case _ => acc

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)
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
}
