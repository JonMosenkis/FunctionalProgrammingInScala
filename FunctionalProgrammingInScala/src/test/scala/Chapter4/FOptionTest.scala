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

  }

}
