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

/**
  * https://en.wikipedia.org/wiki/Contingent_vote
  */
object ContingentMethod extends VoteCountingMethod[WeightedBallot] {

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
    val tls = totals(election, ccandidates)
    val ctSorted: List[(Candidate, Rational)] = tls.toList.sortWith(_._2 > _._2)
    if (ctSorted.head._2 > majorityThreshold * election.length) {
      List(ctSorted.head)
    }
    else {
      val tlsSecondRound = ctSorted.take(2)
      val ccands: List[Candidate] = ccandidates.filterNot(m => m!=tlsSecondRound.head._1 && m!=tlsSecondRound.tail.head._1)
      val secondRoundScores = new MMap[Candidate, Rational]
      for(b<-election if !b.preferences.isEmpty) {
        val preferredCandidate = b.preferences.find(ccands.contains(_))
        preferredCandidate match {
          case Some(c) => secondRoundScores(c) = b.weight + (secondRoundScores.getOrElse(c, 0))
          case _ =>
      }
      }
      List(secondRoundScores.toList.sortWith(_._2 > _._2).head)
    }
  }

}