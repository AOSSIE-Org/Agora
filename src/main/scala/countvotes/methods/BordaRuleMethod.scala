package countvotes.methods

import com.typesafe.scalalogging.{LazyLogging}
import countvotes.structures.{Candidate, Input, Rational, Report, _}
import countvotes.structures.{Candidate, Rational, _}

import collection.mutable.{HashMap => Map}

/**
  * Created by deepeshpandey on 07/03/17.
  */
object BordaRuleMethod extends VoteCountingMethod[WeightedBallot] with LazyLogging{

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int):   Report[WeightedBallot]  = {

    print("\n INPUT ELECTION: \n")
    printElection(election)

    var tls = totals(election, candidates)

    result.addTotalsToHistory(tls)

    report.setCandidates(candidates)
    report.newCount(Input, None, Some(election), Some(tls), None, None)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }

  override def totals(election: Election[WeightedBallot], candidates: List[Candidate]): Map[Candidate, Rational] = {
    val m = new Map[Candidate, Rational]
    val totalCandidates = getCandidates(election).length

    for (b <- election if !b.preferences.isEmpty) {

      // need to take the size of the list first and then calculate the borda scores
     b.preferences.zipWithIndex.foreach(preference => {
       m(preference._1) = m.getOrElse(preference._1, new Rational(0,1)) + ((totalCandidates - 1 - preference._2) * b.weight.numerator.toInt)
     })

    }
    m
  }

  def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int ):
  List[(Candidate,Rational)] = {


    logger.info("Borda rule computation called")

    val tls = totals(election, ccandidates)

    tls.toList.sortWith(_._2 > _._2)

  }

}
