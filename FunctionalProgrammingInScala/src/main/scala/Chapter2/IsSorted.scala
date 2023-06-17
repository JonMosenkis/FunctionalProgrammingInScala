package Chapter2

import scala.annotation.tailrec

object IsSorted {
  def isSorted[A](arr: Array[A], gt: (A, A) => Boolean): Boolean =
    @tailrec
    def fold(index: Int): Boolean =
      if (index >= arr.length)
        true
      else
        val sorted = gt(arr(index-1), arr(index))
        if (sorted)
          fold(index + 1)
        else
          false
      
    arr.length match
      case 1 => true
      case _ => fold(1)
//      case 2 => gt(arr(0), arr(1))
//      case 3 => gt(arr(0), arr(1)) && gt(arr(1), arr(2))
//      case 4 => gt(arr(0), arr(1)) && gt(arr(1), arr(2)) && gt(arr(2), arr(3))
}
