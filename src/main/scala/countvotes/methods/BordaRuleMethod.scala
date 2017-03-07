package countvotes.methods

import countvotes.structures.{Candidate, Input, Rational, Report, _}

import scala.collection.mutable
import scala.collection.mutable.HashMap

/**
  * Created by deepeshpandey on 07/03/17.
  */
object BordaRuleMethod extends BordaRule[WeightedBallot]{

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

  def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int ):
  List[(Candidate,Rational)] = {

    val tls = totals(election, ccandidates)

    tls.toList.sortWith(_._2 > _._2)

  }

}
