package Chapter4

import Chapter4.FEither.{FRight, FLeft}
import org.specs2.mutable.*

class FEitherTest extends Specification {
  "FEither should" >> {
    "map" >> {
      FRight(3).map(_ + 1) must beEqualTo(FRight(4))
      FLeft(3).map(_.toString) must beEqualTo(FLeft(3))
    }

    "flatMap" >> {
      FRight(3).flatMap(_ => FRight(2)) must beEqualTo(FRight(2))
      FLeft(3).flatMap(_ => FRight(2)) must beEqualTo(FLeft(3))
    }
    "orElse" >> {
      FLeft(2).orElse(FRight(3)) must beEqualTo(FRight(3))
      FRight(2).orElse(FRight(3)) must beEqualTo(FRight(2))
    }
    "map2" >> {
      FRight(2).map2(FRight(2))(_ + _) must beEqualTo(FRight(4))
      FLeft("foo").map2(FRight(1))((_, _ )=> 4) must beEqualTo(FLeft("foo"))
      FRight(3).map2(FLeft("foo"))((_, _ )=> 4) must beEqualTo(FLeft("foo"))
      FLeft("foo").map2(FLeft("bar"))((_, _)=> 4) must beEqualTo(FLeft("foo"))
    }
    "traverse" >> {
      def f(i: Int): FEither[String, Int] = if i % 2 == 0 then FRight(i * 2) else FLeft("odd number")
      FEither.traverse(List(2, 3, 4))(f) must beEqualTo(FLeft("odd number"))
      FEither.traverse(List(2, 4, 6))(f) must beEqualTo(FRight(List(4, 8, 12)))
      FEither.traverse(List.empty[Int])(f) must beEqualTo(FRight(List.empty))
    }
    "sequence" >> {
      FEither.sequence(List(FRight(1), FRight(2))) must beEqualTo(FRight(List(1, 2)))
      FEither.sequence(List(FRight(1), FLeft("error"), FRight(3), FLeft("error2"))) must beEqualTo(FLeft("error"))
    }
  }

}
