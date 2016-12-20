#!/usr/bin/env scala

import scala.util.Random

abstract class Door()
case class One() extends Door
case class Two() extends Door
case class Three() extends Door

case class Setup(carBehind: Door, choice: Door, stickWithIt: Boolean)

object MontyHall {
  val seed = new Random
  val possibilities = Vector(One(), Two(), Three())

  def chooseDoor(possibilities: Seq[Door]): Door = {
    possibilities(seed nextInt possibilities.length)
  }

  def chooseFinalDoor(setup: Setup): Door = setup match {
    case Setup(_, choice, true) => choice
    case Setup(a, b, false) =>
      val remaining = possibilities diff (possibilities diff Vector(a, b))
      chooseDoor(remaining)
  }

  def getSetup(stickWithIt: Boolean): Setup = Setup(
    carBehind   = chooseDoor(possibilities),
    choice      = chooseDoor(possibilities),
    stickWithIt = stickWithIt
  )

  def doRun(stickWithIt: Boolean): Boolean = {
    val setup       = getSetup(stickWithIt)
    val finalAnswer = chooseFinalDoor(setup)

    setup.carBehind == finalAnswer
  }

  def percentSuccess(stickWithIt: Boolean, numRuns: Int): Double = {
    val successes = Range(0, numRuns).count(_ => doRun(stickWithIt))

    successes.toDouble / numRuns.toDouble * 100.0
  }

  def main(args: Array[String]): Unit = {
    val stickWithIts = percentSuccess(true, 1000)
    val switchItUps  = percentSuccess(false, 1000)

    println("Success rate for sticking with it? " + stickWithIts + "%")
    println("Success rate for switching it up? " + switchItUps + "%")
  }
}
