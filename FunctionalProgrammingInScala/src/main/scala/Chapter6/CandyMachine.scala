package Chapter6

import Input._

case class CandyMachine(locked: Boolean, candies: Int, coins: Int)

enum Input:
  case Coin, Turn
  
object CandyMachine:
  def simulateMachine(inputs: List[Input]): State[CandyMachine, (Int, Int)] =
    val states = inputs.map(inputToState)
    for
      _ <- State.sequence(states)
      machine <- State.get
    yield (machine.candies, machine.coins)


  def empty: CandyMachine = CandyMachine(true, 0, 0)

  def inputToState(input: Input): State[CandyMachine, Unit] =
    State.modify(singleInput(input))

  def singleInput(input: Input): CandyMachine => CandyMachine =
    machine => updateMachine(input, machine)

  def updateMachine(input: Input, machine: CandyMachine): CandyMachine =
    (input, machine) match
      case (_, CandyMachine(_, 0, _)) => machine
      case (Coin, CandyMachine(true, candy, coins)) => CandyMachine(false, candy, coins+1)
      case (Turn, CandyMachine(false, candy, coins)) => CandyMachine(true, candy-1, coins)
      case _ => machine


