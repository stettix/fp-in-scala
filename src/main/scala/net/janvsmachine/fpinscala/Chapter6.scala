package net.janvsmachine.fpinscala

import States._
import List._

// Exercise 6.11.

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  type MachineState = State[Machine, (Int, Int)]

  def simulateMachine(inputs: List[Input]): MachineState =
    machine => {
      val updated = foldLeft(inputs, machine)((m, input) => handleInput(m, input))
      ((updated.coins, updated.candies), updated)
    }

  def handleInput(machine: Machine, input: Input): Machine =
    input match {
      case Coin if machine.locked && machine.candies > 0 => machine.copy(locked = false, coins = machine.coins + 1)
      case Coin => machine
      case Turn if !machine.locked => machine.copy(locked = true, candies = machine.candies - 1)
      case Turn => machine
    }
}
