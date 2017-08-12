package countvotes.methods


import countvotes.structures._
import countvotes.algorithms._
import countvotes.methods.VoteCountingMethod
import countvotes.methods.InstantExhaustiveBallot.exclude

/**
  * https://en.wikipedia.org/wiki/Exhaustive_ballot#Notes
  */

object InstantExhaustiveDropOffRule extends VoteCountingMethod[WeightedBallot] {

  protected val result: Result = new Result
  protected val report: Report[WeightedBallot] = new Report[WeightedBallot]

  var dropOffPercentage = Rational(0, 100)
  val cutoffPercentage = 25 // drop off of candidates in steps of 5% till 25% after which lowest scoring candidate is eliminated

  def loser(candidate: (Candidate,Rational), total: Int, dropOffPercentage: Rational): Option[(Candidate,Rational)] = {
    if (dropOffPercentage.numerator*(100/dropOffPercentage.denominator) > cutoffPercentage) {
      //println(" 25% dropoff rule crossed, eliminating the last candidate " + candidate._1)
      Some(candidate)
    } else {
      val ccandPercentage = Rational(candidate._2.numerator, total)
      if (ccandPercentage < dropOffPercentage) {
        //println(" Dropping off candidate " + candidate._1 + " for being below " + dropOffPercentage.numerator*100/dropOffPercentage.denominator + "%")
        Some(candidate)
      } else {
        None
      }
    }
  }

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

    val majorityRational = Rational(1, 2)
    val incrememtSize = 5 // drop off percentage increases by 5% in each round of elimination till 25%
    var tls = totals(election, ccandidates).toList.sortWith(_._2 > _._2)
    if (tls.size > 2) {
      if (tls.head._2 > majorityRational * election.size){
        tls.head :: List()
      } else {
        dropOffPercentage = Rational(dropOffPercentage.numerator * (100 / dropOffPercentage.denominator) + incrememtSize, 100)
        val losingCand: Option[(Candidate, Rational)] = loser(tls.last, election.size, dropOffPercentage)
        losingCand match {
          case Some((c,_)) => {
            val newElection = exclude(election, c)
            winners(newElection, ccandidates.filterNot(x => x == c), numVacancies)
          }
          case None => {
            winners(election, ccandidates, numVacancies)
          }
        }
      }
    } else {
      tls.head :: List()
    }
  }
}
