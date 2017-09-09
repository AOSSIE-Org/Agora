package countvotes.methods

import countvotes.structures._

import scala.collection.mutable.{HashSet, HashMap => Map}

abstract class VoteCounter[B <: Ballot] {

  // TODO: Move this to the new election class eventually
  def totals(election: Election[B], candidates: List[Candidate]): Map[Candidate, Rational] = {
    val m = new Map[Candidate, Rational]

    for (c<-candidates) m(c) = 0

    for (b <- election if !b.preferences.isEmpty) {
      m(b.preferences.head) = b.weight + (m.getOrElse(b.preferences.head, 0))
    }
    m
  }
  

  // utility method for matrix where a[i][j] = x means candidate i has got #x votes against candidate j
  def getPairwiseComparisonForWeightedElection(election: Election[Ballot], candidates: List[Candidate]): Array[Array[Rational]] = {

    val zeroRational = Rational(0, 1)
    val responseMatrix = Array.fill(candidates.size, candidates.size)(Rational(0, 1))

    for (b <- election if b.preferences.nonEmpty) {
      b.preferences.zipWithIndex foreach { case (c1,i1) => {
        b.preferences.zipWithIndex foreach { case (c2,i2) => {
          if (i1 < i2) {
            responseMatrix(candidates.indexOf(c1))(candidates.indexOf(c2)) += b.weight
          }}}}}}
    responseMatrix
  }
  
  
  def winners(e: Election[B], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate,Rational)]
}

trait Scrutiny[B <: Ballot] extends VoteCounter[B] {

  protected val result: Result = new Result
  protected val report: Report[B] = new Report[B]

  def runScrutiny(election: Election[B], candidates: List[Candidate], numVacancies: Int):   Report[B]  = {


    var tls = totals(election, candidates)

    result.addTotalsToHistory(tls)

    report.setCandidates(candidates)

    report.newCount(Input, None, None, Some(tls), None, None)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }


}



