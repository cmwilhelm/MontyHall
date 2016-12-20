import scala.util.Random

abstract class Door()
case class One() extends Door
case class Two() extends Door
case class Three() extends Door

case class Setup(carBehind: Door, choice: Door, stickWithIt: Boolean)

object MontyHall {
  val seed = new Random
  val possibilities = Vector(One(), Two(), Three())

  def chooseDoor(): Door = possibilities(seed nextInt possibilities.length)

  def chooseFinalDoor(setup: Setup): Door = setup match {
    case Setup(a, b, false) => {
      val remaining = possibilities diff (possibilities diff Vector(a, b))

      remaining match {
        case Vector(finalDoor) => finalDoor
        case Vector(choice1, choice2) => {
          if (this.seed.nextInt(1) == 1) choice1 else choice2
        }
      }
    }

    case Setup(_, choice, true) => choice
  }

  def getSetup(stickWithIt: Boolean): Setup = Setup(
    carBehind   = chooseDoor(),
    choice      = chooseDoor(),
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
