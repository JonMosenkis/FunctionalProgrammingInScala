package Chapter3

import scala.annotation.tailrec

enum SimpleList[+A]:
  case Nil
  case Cons(head: A, tail: SimpleList[A])



object SimpleList:
  def apply[A](as: A*): SimpleList[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def tail[A](list: SimpleList[A]): SimpleList[A] = list match
    case Cons(_, tail) => tail
    case _ => sys.error("Empty List")

  def setHead[A](head: A, list: SimpleList[A]): SimpleList[A] = list match
    case Cons(_, tail) => Cons(head, tail)
    case Nil => sys.error("Empty List")

  @tailrec
  def drop[A](list: SimpleList[A], n: Int): SimpleList[A] =
    if n <= 0 then list
    else list match
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n-1)

  def dropWhile[A](list: SimpleList[A], predicate: A => Boolean): SimpleList[A] = ???