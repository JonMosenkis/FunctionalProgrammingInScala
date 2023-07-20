package Chapter6

import org.specs2.mutable.*
import CandyMachine._
import Input._

class CandyMachineTest extends Specification {
  "CandyMachine" should {
    "input on empty machined does nothing" >> {
      simulateMachine(List(Coin)).value(empty) must beEqualTo((0, 0))
      singleInput(Coin)(empty) must beEqualTo(empty)
    }
    "coin in locked machine unlocks machine" >> {
      simulateMachine(List(Coin)).state(CandyMachine(true, 1, 1)) must beEqualTo(CandyMachine(false, 1, 2))
    }

    "turning unlocked machine locks and updates contents" >> {
      simulateMachine(List(Turn)).state(CandyMachine(false, 1, 0)) must beEqualTo(CandyMachine(true, 0, 0))
    }

    "inserting and then turning" >> {
      simulateMachine(List(Coin, Turn)).state(CandyMachine(true, 1, 0)) must beEqualTo(CandyMachine(true, 0, 1))
    }

    "turning a locked machine does nothing" >> {
      val machine = CandyMachine(true, 10, 10)
      simulateMachine(List(Turn)).state(machine) must beEqualTo(machine)
    }

    "inserting a coin into an unlocked machine does nothing" >> {
      val machine = CandyMachine(false, 10, 10)
      simulateMachine(List(Coin)).state(machine) must beEqualTo(machine)
    }

    "series of coins and turns" >> {
      val actions = List(Coin, Turn, Turn, Coin, Coin, Turn, Coin, Turn)
      simulateMachine(actions).state(CandyMachine(true, 2, 0)) must beEqualTo(CandyMachine(true, 0, 2))
    }
  }

}
