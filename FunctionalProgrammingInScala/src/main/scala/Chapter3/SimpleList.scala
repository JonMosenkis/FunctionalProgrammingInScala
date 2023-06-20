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

  def dropWhile[A](list: SimpleList[A], predicate: A => Boolean): SimpleList[A] = list match
    case Nil => Nil
    case Cons(head, tail) =>
      if predicate(head) then dropWhile(tail, predicate)
      else Cons(head, dropWhile(tail, predicate))

  def init[A](list: SimpleList[A]): SimpleList[A] = list match
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
    case Nil => Nil

  def foldRight[A, B](list: SimpleList[A], acc: B, f: (A, B) => B): B = foldLeft(reverse(list), acc, (a, b) => f(b, a))

  @tailrec
  def foldLeft[A, B](list: SimpleList[A], acc: B, f: (B, A) => B): B = list match
    case Nil => acc
    case Cons(head, tail) => foldLeft(tail, f(acc, head), f)

  def length[A](list: SimpleList[A]): Int = foldLeft(list, 0, (acc, _) => acc + 1)

  def sum(list: SimpleList[Int]): Int = foldLeft(list, 0, _ + _)

  def product(list: SimpleList[Double]): Double = foldLeft(list, 1.0, _ * _)

  def reverse[A](list: SimpleList[A]): SimpleList[A] = foldLeft(list, Nil: SimpleList[A], (a, b) => Cons(b, a))

  def append[A](listA: SimpleList[A], listB: SimpleList[A]): SimpleList[A] =
    foldRight(listA, listB, (head, tail) => Cons(head, tail))

  def concat[A](lists: SimpleList[SimpleList[A]]): SimpleList[A] = foldRight(lists, Nil: SimpleList[A], append)

  def addOne(list: SimpleList[Int]): SimpleList[Int] = map(list, _+1)

  def asStrings(list: SimpleList[Double]): SimpleList[String] = map(list, _.toString)

  def map[A, B](list: SimpleList[A], f: A => B): SimpleList[B] = foldRight(list, Nil: SimpleList[B], (a, b) => Cons(f(a), b))

  def filter[A](list: SimpleList[A], predicate: A => Boolean): SimpleList[A] =
    def addIf(a: A): SimpleList[A] =
      if predicate(a) then Cons(a, Nil)
      else Nil

    flatMap(list, addIf)

  def flatMap[A, B](list: SimpleList[A], f: A => SimpleList[B]): SimpleList[B] = concat(map(list, f))

  def addTogether(listA: SimpleList[Int], listB: SimpleList[Int]): SimpleList[Int] = (listA, listB) match
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ha, tlA), Cons(hb, tlB)) => Cons(ha + hb, addTogether(tlA, tlB))

  def zipTogether[A, B, C](listA: SimpleList[A], listB: SimpleList[B], f: (A, B) => C): SimpleList[C] =
    @tailrec
    def loop(la: SimpleList[A], lb: SimpleList[B], acc: SimpleList[C]): SimpleList[C] = (la, lb) match
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (Cons(ha, ta), Cons(hb, tb)) => loop(ta, tb, Cons(f(ha, hb), acc))

    reverse(loop(listA, listB, Nil: SimpleList[C]))

  def hasPrefix[A](seq: SimpleList[A], subSeq: SimpleList[A]): Boolean = 
    if length(subSeq) > length(seq) then false
    else
      foldLeft(zipTogether(seq, subSeq, _ == _), true, _ && _)
  
  def hasSubsequence[A](seq: SimpleList[A], subSeq: SimpleList[A]): Boolean =
    @tailrec
    def loop(a: SimpleList[A]): Boolean = a match
      case a if hasPrefix(a, subSeq) => true
      case Nil => false
      case Cons(_, tail) => loop(tail)
      
    loop(seq)
