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

/**
  * https://en.wikipedia.org/wiki/Preferential_block_voting
  */

object PreferentialBlockVoting extends VoteCountingMethod[WeightedBallot] {

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
      val ct = totals1(election, ccandidates1).toList.sortWith(_._2 > _._2)
      if (ct.head._2 > majorityThreshold * election.length && ccandidates1.length > vacancies) {
        winnerlist = (ct.head._1, ct.head._2) :: winnerlist
        vacancies -= 1
        ccandidates1 = ccandidates1.filter(_ != ct.head._1)
      } else if (ccandidates1.length == vacancies) {
        winnerlist = ct ::: winnerlist
        vacancies = 0
      } else {
        ccandidates1 = ccandidates1.filter(_ != ct.reverse.head._1)
      }
    }
    winnerlist
  }

  def totals1(election: Election[WeightedBallot], candidates: List[Candidate]): MMap[Candidate, Rational] = {
    val mp = new MMap[Candidate, Rational]
    for (b <- election if !b.preferences.isEmpty) {
      val preferredCandidate = b.preferences.find(candidates.contains(_))
      preferredCandidate match {
        case Some(c) => mp(c) = b.weight + (mp.getOrElse(c, 0))
        case _ =>
      }
    }
    mp
  }
}

