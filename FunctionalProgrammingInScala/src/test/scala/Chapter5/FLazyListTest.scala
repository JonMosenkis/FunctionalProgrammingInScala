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
    "forAll" >> {
      val counter = new CallCounter()
      val predicate = counter.setFunction[Int, Boolean](_ % 2 == 0)

      FLazyList(2, 4, 6).forAll(predicate) must beTrue
      counter.counts must beEqualTo(3)

      counter.reset()
      FLazyList(2, 3, 6).forAll(predicate) must beFalse
      counter.counts must beEqualTo(2)
    }
  }

}
class CallCounter() {
  var counts = 0

  def setFunction[A, B](f: A => B): A => B = call(_)(f)

  def call[A, B](a: A)(f: A => B): B =
    counts += 1
    f(a)

  def reset(): Unit = counts = 0
}
