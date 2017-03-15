package countvotes.methods

import countvotes.structures.{Candidate, Input, Rational, Report, Result, WeightedBallot, _}
import countvotes.structures.{Candidate, Input, Rational, Report, _}
import countvotes.structures.{Candidate, Rational, _}

import collection.mutable.{ListBuffer, HashMap => Map}

/**
  * Created by deepeshpandey on 14/03/17.
  */
object CoombRuleMethod extends VoteCountingMethod[WeightedBallot]{

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]
  private val majorityThreshold = Rational(1,2)

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

    var winnerMap = new Map[Candidate, Rational]
    var coombsCandidateMap = new Map[Candidate, (Rational, Rational)]

    for (b <- election if !b.preferences.isEmpty) {

      b.preferences.filter(firstRankCandidate => ccandidates.contains(firstRankCandidate)).take(1).foreach(firstCandidate => {

        var firstCandidateValue = coombsCandidateMap.getOrElse(firstCandidate, getZeroTuple())
        var firstCandidateNewValue = firstCandidateValue.copy(_1 = firstCandidateValue._1 + b.weight)
        coombsCandidateMap.update(firstCandidate, firstCandidateNewValue)

        if (firstCandidateNewValue._1 > majorityThreshold * getTotalVoters(election)) {
          winnerMap(firstCandidate) = firstCandidateNewValue._1
          return winnerMap.toList
        }
      })

      b.preferences.reverse.filter(lastRankCandidate => ccandidates.contains(lastRankCandidate)).take(1).foreach(lastCandidate => {
        var lastCandidateValue = coombsCandidateMap.getOrElse(lastCandidate, getZeroTuple())
        coombsCandidateMap.update(lastCandidate, lastCandidateValue.copy(_2 = lastCandidateValue._2 + b.weight))
      })
    }

      // check if there is a tie in last ranked values
      val lastRankValueList = coombsCandidateMap.toList.sortWith(_._2._2 < _._2._2)

    if (lastRankValueList.head._2._2 == lastRankValueList.last._2._2) {
        coombsCandidateMap.toList.foreach(candidiateMap => {
          winnerMap(candidiateMap._1) = candidiateMap._2._1
        })
        return winnerMap.toList
      }

      // remove the minimum ranked alternative from the candidates list and recurse
      val maximumLowestRankedCandidate = lastRankValueList.maxBy(_._2._2)
      winners(election, ccandidates.filter(_ != maximumLowestRankedCandidate._1), numVacancies)
  }

  def getZeroTuple() : (Rational, Rational) = {
    (new Rational (0, 1), new Rational(0, 1))
  }

}
