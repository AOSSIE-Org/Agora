package countvotes.methods

import countvotes.structures._
import countvotes.algorithms._
import scala.math._

object BrunoMethod extends BrunoMethodClass[ACTBallot] {

  def runScrutiny(election: Election[ACTBallot], numVacancies: Int):  Report[ACTBallot] = {
    println("Number of ballots: " + election.length)

    //Last parameter activates the dynamic programming approach when true
    report.setWinners(computeWinners(election, numVacancies, false))
    report
  }
}
