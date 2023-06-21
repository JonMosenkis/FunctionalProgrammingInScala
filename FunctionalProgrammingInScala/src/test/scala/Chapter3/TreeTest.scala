package Chapter3

import Chapter3.Tree.{Branch, Leaf}
import org.specs2.mutable.*

class TreeTest extends Specification {
  "Tree should" >> {

    "leftMostValue" >> {
      Branch(Leaf(1), Branch(Leaf(2), Leaf(1))).leftMostValue() must beEqualTo(1)
    }

    "find maximum value" >> {
      Branch(Leaf(1), Branch(Leaf(2), Leaf(1))).maximum() must beEqualTo(2)
    }

    "depth" >> {
      Branch(Leaf(1), Branch(Leaf(2), Branch(Branch(Leaf(1), Leaf(1)), Leaf(2)))).depth() must beEqualTo(5)
    }
    
    "map" >> {
      Branch(Leaf(1), Branch(Leaf(2), Leaf(3))).map(_+1) must beEqualTo(Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))
    }
  }

}
