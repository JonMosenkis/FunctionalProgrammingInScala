package Chapter5

import org.specs2.mutable.*

class FLazyListTest extends Specification {
  "FLazyList should" >> {
    "toList" >> {
      FLazyList(1, 2, 3).toList must beEqualTo(List(1, 2, 3))
    }
  }

}
