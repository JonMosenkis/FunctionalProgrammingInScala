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

    "pop" >> {
      SimpleList.init(SimpleList(1, 2, 3, 4)) must beEqualTo(SimpleList(1, 2, 3))
    }

    "calculate length" >> {
      SimpleList.length(SimpleList("a", "b", "c", "c")) must beEqualTo(4)
    }

    "foldLeft" >> {
      SimpleList.foldLeft(SimpleList(3, 2, 1), 6, _ - _) must beEqualTo(0)
    }

    "foldRight" >> {
      SimpleList.foldRight(SimpleList(1, 2, 3), Nil: SimpleList[Int], Cons(_, _)) must beEqualTo(SimpleList(1, 2, 3))
    }

    "sum" >> {
      SimpleList.sum(SimpleList(1, 2, 3)) must beEqualTo(6)
    }

    "product" >> {
      SimpleList.product(SimpleList(2.0, 2.0, 3.0)) must beEqualTo(12.0)
    }

    "reverse" >> {
      SimpleList.reverse(SimpleList('a', 'b', 'c')) must beEqualTo(SimpleList('c', 'b', 'a'))
    }

    "append" >> {
      SimpleList.append(SimpleList(1, 2, 3), SimpleList(4, 5, 6)) must beEqualTo(SimpleList(1, 2, 3, 4, 5, 6))
    }

    "concat" >> {
      SimpleList.concat(SimpleList(SimpleList(1, 2), SimpleList(3, 4), SimpleList(5, 6))) must beEqualTo(SimpleList(1, 2, 3, 4, 5, 6))
    }
  }



}
