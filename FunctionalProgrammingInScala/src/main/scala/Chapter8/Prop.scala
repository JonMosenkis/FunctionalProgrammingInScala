package Chapter8

import Chapter8.Prop.{FailedCase, SuccessCount}

trait Prop {
  self =>
    def check: Either[(FailedCase, SuccessCount), SuccessCount]
    def &&(that: Prop): Prop = new Prop:
      override def check: Either[(FailedCase, SuccessCount), SuccessCount] = for {
        selfSuccesses <- self.check
        thatSuccesses <- that.check
      } yield selfSuccesses + thatSuccesses
}

object Prop {
  opaque type SuccessCount = Int
  opaque type FailedCase = String

  extension (x: SuccessCount)
    def +(y: SuccessCount): SuccessCount = x + y

}
