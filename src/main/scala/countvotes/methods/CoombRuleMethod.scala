package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures._

import collection.mutable.{ListBuffer, HashMap => MMap}

/**
  * Algorithm : https://en.wikipedia.org/wiki/Coombs%27_method
  */
object CoombRuleMethod extends VoteCountingMethod[WeightedBallot] with LazyLogging{

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]
  private val majorityThreshold = Rational(1,2)

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int):   Report[WeightedBallot]  = {

    print("\n INPUT ELECTION: \n")
    printElection(election)

    report.setCandidates(candidates)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }


  def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int ):
  List[(Candidate,Rational)] = {

    logger.info("computing coomb winner")

    val firstRankedMap = new MMap[Candidate, Rational]
    val lastRankedMap = new MMap[Candidate, Rational]
    val totalVoters = Election.totalWeightedVoters(election)

    for (e <- election if e.preferences.nonEmpty) {

      val firstRankedCandidate = e.preferences.filter(ccandidates.contains(_)).take(1).head
      val lastRankedCandidate = e.preferences.reverse.filter(ccandidates.contains(_)).take(1).head

      firstRankedMap(firstRankedCandidate) = firstRankedMap.getOrElse(firstRankedCandidate, Rational(0, 1)) + e.weight
      lastRankedMap(lastRankedCandidate) = lastRankedMap.getOrElse(lastRankedCandidate, Rational(0, 1)) + e.weight
    }


    if(firstRankedMap.maxBy(_._2)._2 > Rational(1, 2) * totalVoters) {
      List(firstRankedMap.maxBy(_._2))
    } else {
      winners(election, ccandidates.filter {_ != lastRankedMap.maxBy(_._2)._1}, numVacancies)
    }

  }

}
