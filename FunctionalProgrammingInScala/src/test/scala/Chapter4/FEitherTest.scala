package Chapter4

import Chapter4.FEither.{FRight, FLeft}
import org.specs2.mutable.*

class FEitherTest extends Specification {
  "FEither should" >> {
    "map" >> {
      FRight(3).map(_ + 1) must beEqualTo(FRight(4))
      FLeft(3).map(_.toString) must beEqualTo(FLeft(3))
    }

    "flatMap" >> {
      FRight(3).flatMap(_ => FRight(2)) must beEqualTo(FRight(2))
      FLeft(3).flatMap(_ => FRight(2)) must beEqualTo(FLeft(3))
    }
    "orElse" >> {
      FLeft(2).orElse(FRight(3)) must beEqualTo(FRight(3))
      FRight(2).orElse(FRight(3)) must beEqualTo(FRight(2))
    }
    "map2" >> {
      FRight(2).map2(FRight(2))(_ + _) must beEqualTo(FRight(4))
      FLeft("foo").map2(FRight(1))((_, _ )=> 4) must beEqualTo(FLeft("foo"))
      FRight(3).map2(FLeft("foo"))((_, _ )=> 4) must beEqualTo(FLeft("foo"))
      FLeft("foo").map2(FLeft("bar"))((_, _)=> 4) must beEqualTo(FLeft("foo"))
    }
  }

}
