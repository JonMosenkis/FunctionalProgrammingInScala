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
    "headOption" >> {
      FLazyList(1, 2, 3).headOption() must beSome(1)
      FLazyList.empty.headOption() must beNone
    }
    "append" >> {
      FLazyList(1, 2).append(FLazyList(3, 4)).toList must beEqualTo(List(1, 2, 3, 4))
    }
    "filter" >> {
      FLazyList(1, 2 ,3 ,4).filter(_ < 3).toList must beEqualTo(List(1, 2))
    }
    "map" >> {
      FLazyList(1, 2, 3, 4).map(_ * 2).toList must beEqualTo(List(2, 4, 6, 8))
    }
    "flatMap" >> {
      FLazyList(1, 2, 3).flatMap(x => FLazyList(x*2, x*2)).toList must beEqualTo(List(2, 2, 4, 4, 6, 6))
    }
    "continually" >> {
      FLazyList.continually(2).take(3).toList must beEqualTo(List(2, 2, 2))
    }
    "from" >> {
      FLazyList.from(3).take(3).toList must beEqualTo(List(3, 4, 5))
    }
    "fibs" >> {
      FLazyList.fibs().take(7).toList must beEqualTo(List(0, 1, 1, 2, 3, 5, 8))
    }
    "zipAll" >> {
      FLazyList(1, 2, 3).zipAll(FLazyList("foo", "bar")).toList must beEqualTo(
        List((Some(1), Some("foo")), (Some(2), Some("bar")), (Some(3), None))
      )
    }
    "startsWith" >> {
      FLazyList(1, 2, 3).startsWith(FLazyList(1, 2)) must beTrue
      FLazyList(1, 2, 3).startsWith(FLazyList(2, 3)) must beFalse
      FLazyList(1, 2, 3).startsWith(FLazyList.empty) must beTrue
    }
    "tails" >> {
      FLazyList(1, 2, 3).tails().map(_.toList).toList must beEqualTo(List(
        List(1, 2 ,3),
        List(2, 3),
        List(3),
        List()
      ))
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
