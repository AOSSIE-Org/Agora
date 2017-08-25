package analyse.methods

import countvotes.structures.{Candidate, Election, Rational, WeightedBallot}

import scala.util.Random

abstract class StabilityAnalysis {

  // generate n random election of m voters and c candidates
  def generateElections(n: Int, m: Int, c: Int): List[Election[WeightedBallot]] = {

    require(c < 26)

    val candidates = ('A' to 'Z') map (name => new Candidate(name.toString)) take c toList

    val election = for {
      i <- List.range(1, n)
    } yield {
      for {
        j <- List.range(1, m)
      } yield WeightedBallot(Random.shuffle(candidates), i, 1)
    }

    election
  }

  // calculate the kendall tau distance between two profiles
  def kendallTauDistance(profile: List[Election[WeightedBallot]]): Int = {

    var kTDistance = 0

    profile(0) zip profile(1) foreach {case (we1, we2) => {
      we1.preferences.foreach(c1 => {
        we1.preferences.foreach(c2 => {
          if ((we1.preferences.indexOf(c1) < we1.preferences.indexOf(c2)) && (we2.preferences.indexOf(c1) > we2.preferences.indexOf(c2))) {
            kTDistance += 1
          }
        })
      })
    }}

    kTDistance
  }

  // http://www.nature.com/nature/journal/v234/n5323/abs/234034a0.html?foxtrotcallback=true
  def winnerSetComparison(winnerEA: List[Candidate], winnerEB: List[Candidate]): Rational = {

    ((winnerEA intersect winnerEB).distinct length) / (winnerEA union winnerEB).distinct.length

  }

  def analyse(): Unit

}
