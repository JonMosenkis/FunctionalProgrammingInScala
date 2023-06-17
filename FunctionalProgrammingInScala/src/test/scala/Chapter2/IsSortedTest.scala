package Chapter2

import org.specs2.mutable.*
import Chapter2.IsSorted.isSorted


class IsSortedTest extends Specification {
  "isSorted should" >> {
    "single item sorted" >> {
      isSorted(Array(1), (_ < _)) must beTrue
    }
    "(2, 1) not sorted" >> {
      isSorted(Array(2, 1), (_ < _)) must beFalse
    }
    "(1, 2) sorted" >> {
      isSorted(Array(1, 2), (_ < _)) must beTrue
    }
    "(1, 2 ,3) sorted" >> {
      isSorted(Array(1, 2, 3), (_ < _)) must beTrue
    }
    "(2, 1, 3) not sorted" >> {
      isSorted(Array(2, 1, 3), (_ < _)) must beFalse
    }
    "(2, 1, 5, 3) not sorted" >> {
      isSorted(Array(2, 1, 5, 3), (_ < _)) must beFalse
    }
    "sorted by string length" >> {
      isSorted(Array("a", "aa", "aaa"), (_.length < _.length)) must beTrue
    }
  }

}
