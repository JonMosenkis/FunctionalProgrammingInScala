package Chapter5

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
