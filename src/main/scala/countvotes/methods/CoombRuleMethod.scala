package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures._

import collection.mutable.{ListBuffer, HashMap => MMap}

/**
  * Algorithm : https://en.wikipedia.org/wiki/Coombs%27_method
  * Note: This voting method requires voters to rank all the candidates
  */
object CoombRuleMethod extends VoteCountingMethod[WeightedBallot] with LazyLogging{

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]
  private val majorityThreshold = Rational(1,2)

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int):   Report[WeightedBallot]  = {

    require(election forall(b => b.preferences.length == candidates.length))

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

    // check if there is a majority winner

    for (e <- election if e.preferences.nonEmpty) {

      val firstRankedCandidate = e.preferences.find(c => ccandidates.contains(c))

      firstRankedCandidate match {
        case Some(candidate) => firstRankedMap(candidate) = firstRankedMap.getOrElse(candidate, Rational(0, 1)) + e.weight
        case None => {}
      }
    }

    if(firstRankedMap.maxBy(_._2)._2 > Rational(1, 2) * totalVoters) {

      List(firstRankedMap.maxBy(_._2))

    } else {
      // winner not found create the last ranked map and filter the highest last ranked candidate

      for (e <- election if e.preferences.nonEmpty) {
        val lastRankedCandidate = e.preferences.reverseIterator.find(c => ccandidates.contains(c))

        lastRankedCandidate match {
          case Some(candidate) => lastRankedMap(candidate) = lastRankedMap.getOrElse(candidate, Rational(0, 1)) + e.weight
          case None => {}
        }

      }
      winners(election, ccandidates.filter {_ != lastRankedMap.maxBy(_._2)._1}, numVacancies)
    }

  }

}
