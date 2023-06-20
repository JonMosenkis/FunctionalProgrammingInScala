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

    "addOne" >> {
      SimpleList.addOne(SimpleList(1, 2, 4)) must beEqualTo(SimpleList(2, 3, 5))
    }

    "asString" >> {
      SimpleList.asStrings(SimpleList(1.0, 2.0)) must beEqualTo(SimpleList("1.0", "2.0"))
    }

    "filter" >> {
      SimpleList.filter(SimpleList(1, 2, 3, 4, 5), _ % 2 == 0)  must beEqualTo(SimpleList(2, 4))
    }

    "flatMap" >> {
      SimpleList.flatMap(SimpleList(1, 2), a => SimpleList(a, a)) must beEqualTo(SimpleList(1, 1, 2, 2))
    }

    "addTogether" >> {
      SimpleList.addTogether(SimpleList(1, 2, 3), SimpleList(4, 5, 6)) must beEqualTo(SimpleList(5, 7, 9))
    }

    "zipTogether" >> {
      SimpleList.zipTogether(SimpleList(1, 2), SimpleList("a", "b"), (n, l) => s"$l${n.toString}") must beEqualTo(
        SimpleList("a1", "b2")
      )
    }

    "hasPrefix" >> {
      SimpleList.hasPrefix(SimpleList(1, 2, 3, 4), SimpleList(1, 2)) must beTrue
      SimpleList.hasPrefix(SimpleList(1, 2, 3, 4), SimpleList(1, 3)) must beFalse
    }

    "hasSubsequence" >> {
      SimpleList.hasSubsequence(SimpleList(1, 2, 3, 4), SimpleList(2, 3)) must beTrue
      SimpleList.hasSubsequence(SimpleList(1, 2), SimpleList(8)) must beFalse
    }
  }



}
