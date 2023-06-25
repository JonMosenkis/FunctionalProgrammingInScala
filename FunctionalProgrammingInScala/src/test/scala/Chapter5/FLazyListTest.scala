package Chapter5

import org.specs2.mutable.*

class FLazyListTest extends Specification {
  "FLazyList should" >> {
    "toList" >> {
      FLazyList(1, 2, 3).toList must beEqualTo(List(1, 2, 3))
    }
    "take" >> {
      FLazyList(1, 2, 3).take(2).toList must beEqualTo(List(1, 2))
      FLazyList(1, 2, 3).take(0).toList must beEqualTo(Nil)
      FLazyList(1, 2, 3).take(4).toList must beEqualTo(List(1, 2, 3))
    }
    "drop" >> {
      FLazyList(1, 2, 3, 4).drop(2).toList must beEqualTo(List(3, 4))
      FLazyList(1, 2, 3).drop(0).toList must beEqualTo(List(1, 2, 3))
      FLazyList(1, 2).drop(7).toList must beEqualTo(Nil)
    }
    "takeWhile" >> {
      FLazyList(1, 3, 5, 6, 7).takeWhile(_ % 2 == 1).toList must beEqualTo(List(1, 3 ,5))
      FLazyList.empty[Int].takeWhile(_ % 2 == 0).toList must beEqualTo(Nil)
      FLazyList(1, 2, 3).takeWhile(_ < 0).toList must beEqualTo(Nil)
    }
  }

}
