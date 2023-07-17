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


  def intDouble: Rand[(Int, Double)] =
    both(int, double)

  def doubleInt: Rand[(Double, Int)] =
    both(double, int)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def int: Rand[Int] = (rng: RNG) => rng.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    (rng: RNG) =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(List.empty[A]))((a, b) => append(a, b))

  def append[A](a: Rand[A], rl: Rand[List[A]]): Rand[List[A]] =
    map2(a, rl)((value, list) => value :: list)

}

object Foo extends App {
  val v = double3(SimpleRNG(42))
  println((v._1._1, v._1._2, v._1._3))

}


