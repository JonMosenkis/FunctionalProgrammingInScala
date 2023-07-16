package Chapter6

import Chapter6.RNG.{double3, double}

import scala.annotation.tailrec

type Rand[+A] = RNG => (A, RNG)

trait RNG {
  def nextInt: (Int, RNG)


}

case class SimpleRNG(seed: Long) extends RNG:
  override def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5DEECE66DL + 0XBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)


object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match
    case (n, rng) => if n < 0 then (-1 * (n + 1), rng) else (n, rng)

  def double: Rand[Double] =
    map(nonNegativeInt)(n => n.toDouble / Int.MaxValue.toDouble + 1)


  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val (d, rng1) = double(rng)
    val (i, rng2) = rng1.nextInt
    ((d, i), rng2)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    @tailrec
    def loop(l: List[Int], r: RNG): (List[Int], RNG) =
      val next = r.nextInt
      val nextIter = (next._1 +: l, next._2)
      if nextIter._1.length == count then nextIter
      else loop(nextIter._1, nextIter._2)

    loop(List.empty, rng)

  val int: Rand[Int] = (rng: RNG) => rng.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    (rng: RNG) =>
      val (a, rng2) = s(rng)
      (f(a), rng2)


}

object Foo extends App {
  val v = double3(SimpleRNG(42))
  println((v._1._1, v._1._2, v._1._3))

}

