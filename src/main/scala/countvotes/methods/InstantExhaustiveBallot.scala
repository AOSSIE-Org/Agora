package countvotes.methods

import countvotes.structures._
import countvotes.algorithms._


import countvotes.methods.VoteCountingMethod

/**
  * https://en.wikipedia.org/wiki/Exhaustive_ballot
  */

object InstantExhaustiveBallot extends VoteCountingMethod[WeightedBallot] {

  protected val result: Result = new Result
  protected val report: Report[WeightedBallot] = new Report[WeightedBallot]

  def exclude(election: Election[WeightedBallot], candidate: Candidate): Election[WeightedBallot] = {
    election map { b =>
      val newPrefs = b.preferences filter { _ != candidate }
      WeightedBallot(newPrefs, b.id, b.weight)
    }
  }
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

  override def winners(election: Election[WeightedBallot], ccandidates: List[Candidate],  numVacancies: Int): List[(Candidate, Rational)] = {

    val ct = totals(election,ccandidates)
    if (ct.size > 2) {
      val losingCand =  ct.toList.sortWith(_._2 < _._2).head
      val newElection = exclude(election, losingCand._1)
      winners(newElection, ccandidates.filter(_ != losingCand._1), numVacancies)
    } else {
      ct.toList.sortWith(_._2 > _._2).head::List()
    }
  }
}
