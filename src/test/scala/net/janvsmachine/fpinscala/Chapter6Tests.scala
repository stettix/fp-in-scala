package net.janvsmachine.fpinscala

import org.scalatest.FlatSpec

import Machine._
import States._

class MachineTests extends FlatSpec {

  val emptyMachine = Machine(true, 0, 0)
  val machineWithCandy = Machine(true, 10, 0)

  "An empty candy dispenser" should "ignore all inputs" in {
    val allInputs = Seq(Nil, List(Coin), List(Turn))
    for (inputs <- allInputs) {
      val ((coins, candies), _) = simulateMachine(inputs)(emptyMachine)
      assert(coins == 0 && candies == 0)
    }
  }

  "A machine with candy" should "give candy when coin is inserted and knob is turned" in {
    val ((coins, candies), _) = simulateMachine(List(Coin, Turn))(machineWithCandy)
    assert(coins == machineWithCandy.coins + 1 &&
      candies == machineWithCandy.candies - 1)
  }

  it should "not respond to turning knob when no coin has been given" in {
    val ((coins, candies), _) = simulateMachine(List(Turn))(machineWithCandy)
    assert(coins == machineWithCandy.coins && candies == machineWithCandy.candies)
  }

  it should "not give candy until knob is turned" in {
    val ((coins, candies), _) = simulateMachine(List(Coin))(machineWithCandy)
    assert(coins == machineWithCandy.coins + 1 && candies == machineWithCandy.candies)
  }

  it should "dispense candy after several input sequences" in {
      val ((coins, candies), _) = simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn))(machineWithCandy)
      assert(coins == machineWithCandy.coins + 3 && candies == machineWithCandy.candies - 3)
  }

}
