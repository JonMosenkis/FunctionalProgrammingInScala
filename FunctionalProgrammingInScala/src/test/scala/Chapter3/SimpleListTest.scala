package Chapter3

import org.specs2.mutable.*
import Chapter3.SimpleList.*

class SimpleListTest extends Specification {
  "SimpleList should" >> {
    "drop" >> {
      SimpleList.drop(SimpleList(1, 2, 3), 2) must beEqualTo(SimpleList(3))
    }

    "dropWhile" >> {
      SimpleList.dropWhile(SimpleList(1, 2, 3, 4), a => a % 2 == 0) must beEqualTo(SimpleList(1 ,3))
    }
  }



}
