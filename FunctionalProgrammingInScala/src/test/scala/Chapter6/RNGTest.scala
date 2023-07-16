package Chapter6

import org.specs2.mutable.*

class RNGTest extends Specification {
  "RNG" should {
    "ints" >> {
      val length = 5
      RNG.ints(length)(SimpleRNG(42))._1.length must beEqualTo(length)
    }
  }

}
