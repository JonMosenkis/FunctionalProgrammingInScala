package Chapter4

import Chapter4.FOption.{FNone, FSome}
import org.specs2.mutable.*

class FOptionTest  extends Specification {
  "FOption should" >> {
    "map" >> {
      FOption(1).map(_ + 1) must beEqualTo(FSome(2))
    }
    "getOrElse" >> {
      FSome(3).getOrElse(6) must beEqualTo(3)
      FNone.getOrElse(6) must beEqualTo(6)
    }
    "flatMap" >> {
      FSome(3).flatMap(_ => FNone) must beEqualTo(FNone)
      FSome(3).flatMap(a => FSome(a + 1)) must beEqualTo(FSome(4))
    }
    "filter" >> {
      FSome(3).filter(_ == 2) must beEqualTo(FNone)
      FSome(3).filter(_ == 3) must beEqualTo(FSome(3))
    }
    "orElse" >> {
      FSome(3).orElse(FNone) must beEqualTo(FSome(3))
      FNone.orElse(FSome(4)) must beEqualTo(FSome(4))
    }
    "mean" >> {
      FOption.mean(Seq(1.0, 2.0, 3.0)) must beEqualTo(FSome(2.0))
      FOption.mean(Nil) must beEqualTo(FNone)
    }
    "variance" >> {
      FOption.variance(Seq(1, 2, -2, 4, -3).map(_.toDouble)).getOrElse(-1.0) must beCloseTo(6.64, 0.01)
      FOption.variance(Seq.empty[Double]) must beEqualTo(FNone)
    }

  }

}
