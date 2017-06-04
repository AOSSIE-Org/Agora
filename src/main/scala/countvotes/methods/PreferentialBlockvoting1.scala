package countvotes.methods


import countvotes.structures._
import countvotes.algorithms._

import scala.collection.immutable.ListMap
import collection.mutable.{HashMap => MMap}
import scala.collection.SortedMap
import collection.breakOut
import scala.util.Random
import scala.util.Sorting
import java.io._

import countvotes.methods.VoteCountingMethod

object PreferentialBlockvoting1 extends VoteCountingMethod[WeightedBallot] {

  protected val result: Result = new Result
  protected val report: Report[WeightedBallot] = new Report[WeightedBallot]

  val majorityThreshold = Rational(1, 2)

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int): Report[WeightedBallot] = {


    print("\n INPUT ELECTION: \n")
    printElection(election)

    val tls = totals(election, candidates) // Here are totals of candidates also not OCCURING in the ballots

    result.addTotalsToHistory(tls)

    //report.setCandidates(getCandidates(election))  // Here are candidates OCCURING in the election
    report.setCandidates(candidates) // Here are candidates also not OCCURING in the election

    report.newCount(Input, None, Some(election), Some(tls), None, None)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  override def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {
    var winnerlist: List[(Candidate, Rational)] = Nil
    var vacancies = numVacancies
    var ccandidates1 = ccandidates
    while (vacancies != 0) {
      var tls = totals1(election, ccandidates1)
      var tls1: List[(Candidate, Rational)] = tls.toList.sortWith(_._2 > _._2)
      if (tls1.head._2 > majorityThreshold * election.length && ccandidates1.length > vacancies) {
        winnerlist = (tls1.head._1, tls1.head._2) :: winnerlist
        vacancies = vacancies - 1
        ccandidates1 = ccandidates1.filter(_ == tls.head._1)
      } else if (ccandidates1.length == vacancies) {
        winnerlist = tls1 ::: winnerlist
        vacancies = 0
      } else {
        ccandidates1 = ccandidates1.filter(_ != tls1.reverse.head._1)
      }
    }
    winnerlist
  }

  def totals1(election: Election[WeightedBallot], candidates: List[Candidate]): MMap[Candidate, Rational] = {
    var mp = new MMap[Candidate, Rational]

    for (c <- candidates) mp(c) = 0

    for (b <- election if !b.preferences.isEmpty) {
      b.preferences.find(m => candidates.contains(m)).foreach(m => mp(m) = mp(m) + 1)
    }
    mp
  }
}

