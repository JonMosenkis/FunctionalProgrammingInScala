package Chapter6

import Chapter6.RNG.{double3, randomDouble}

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

  def randomDouble(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match
    case (n, rng) => (n.toDouble / Int.MaxValue.toDouble, rng)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, rng1) = rng.nextInt
    val (d, rng2) = randomDouble(rng1)
    ((i, d), rng2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val (d, rng1) = randomDouble(rng)
    val (i, rng2) = rng1.nextInt
    ((d, i), rng2)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, rng1) = randomDouble(rng)
    val (d2, rng2) = randomDouble(rng1)
    val (d3, rng3) = randomDouble(rng2)
    ((d1, d2, d3), rng3)
}

object Foo extends App {
  val v = double3(SimpleRNG(42))
  println((v._1._1, v._1._2, v._1._3))

}


