package countvotes.methods


import countvotes.structures._
import countvotes.algorithms._

import scala.collection.immutable.ListMap
import collection.mutable.{HashMap => MMap}
import scala.collection.SortedMap
import collection.mutable.HashSet
import collection.breakOut
import scala.util.Random
import scala.util.Sorting
import java.io._

import countvotes.methods.VoteCountingMethod

/***
  * https://en.wikipedia.org/wiki/Oklahoma_primary_electoral_system
  */

object OklahomaMethod extends VoteCountingMethod[WeightedBallot] {

  protected val result: Result = new Result
  protected val report: Report[WeightedBallot] = new Report[WeightedBallot]

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int):   Report[WeightedBallot]  = {



    print("\n INPUT ELECTION: \n")
    printElection(election)

    val tls = totals(election, candidates) // Here are totals of candidates also not OCCURING in the ballots

    result.addTotalsToHistory(tls)

    //report.setCandidates(getCandidates(election))  // Here are candidates OCCURING in the election
    report.setCandidates(candidates)  // Here are candidates also not OCCURING in the election

    report.newCount(Input, None, Some(election), Some(tls), None, None)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  // following recursive function calculates totals and if total of any candidate exceeds half of election length
  // candidate wins, else next preferences are added, reducing their weights by 1/N,
  // where N denotes Nth preference on the ballot
  // that is, 1st preference has weight 1, 2nd preference has weight 1/2. 3rd preference has weight 1/3 and so on
  def oklahomaTotals(election: Election[WeightedBallot], ccandidates: List[Candidate], ccandScoreMap: MMap[Candidate, Rational], multiplier: Rational): List[(Candidate, Rational)] = {
    val candidateScoreMap = ccandScoreMap
    val candidateTotalScores = totals(election, ccandidates)
    for (c<-ccandidates) {
      candidateScoreMap(c) = candidateScoreMap.getOrElse(c, Rational(0, 1)) + candidateTotalScores.getOrElse(c, Rational(0, 1)) * multiplier
    }
    if (candidateScoreMap.toList.sortWith(_._2 > _._2).head._2 > (election.length / 2)) {
      candidateScoreMap.toList.sortWith(_._2 > _._2).head :: List()
    } else {
      var newElection: Election[WeightedBallot] = Nil
      for (b<-election) {
        newElection = WeightedBallot(b.preferences.tail, b.id, b.weight) :: newElection
      }
      oklahomaTotals(newElection, ccandidates, candidateScoreMap, Rational(multiplier.numerator, multiplier.denominator + 1))
    }
  }

  override def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {
    var ccandidateScoreMap = new MMap[Candidate, Rational]
    var winner = oklahomaTotals(election, ccandidates, ccandidateScoreMap, Rational(1,1))
    winner
  }
}
