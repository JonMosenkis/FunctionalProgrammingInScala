package Chapter2

import scala.annotation.tailrec

object Fibonacci {
  @main def main(): Unit = println(fib(19))


  def fib(n: Int): Int =
    @tailrec
    def go(value: Int, prev: Int, prePrev: Int): Int =
      val current = prev + prePrev
      if (value == n)
        current
      else
        go(value + 1, current, prev)

    if (n <= 1)
      n
    else
      go(2, 1, 0)

}
