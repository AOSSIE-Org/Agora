package countvotes.methods

import countvotes.structures._

import collection.mutable.{ListBuffer, HashMap => MMap}

/**
  * Created by deepeshpandey on 21/03/17.
  */
object MinimaxCondorcetMethod extends VoteCountingMethod[WeightedBallot] {

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]
  private val rational0 = Rational(0, 1)
  private val majorityThreshold = Rational(1, 2)

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int): Report[WeightedBallot] = {

    print("\n INPUT ELECTION: \n")
    printElection(election)

    var tls = totals(election, candidates)

    result.addTotalsToHistory(tls)

    report.setCandidates(candidates)
    report.newCount(Input, None, Some(election), Some(tls), None, None)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }

  def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int):
  List[(Candidate, Rational)] = {

    val tallyTable = new MMap[MMap[Candidate, Candidate], Rational]
    val winnerMap = new MMap[Candidate, Rational]
    val candidatePairKey = new MMap[Candidate, Candidate]
    val totalVoters = getTotalVoters(election)

    // initialise the tally table
    ccandidates.foreach { c1 => {
      ccandidates.foreach { c2 => {
        if (c1 != c2) {
          var candidatePair = new MMap[Candidate, Candidate]
          candidatePair(c1) = c2
          tallyTable(candidatePair) = 0
        }
      }
      }
    }
    }

    // update the tally table using election
    for (b <- election if !b.preferences.isEmpty) {
      val voterPreference = b.preferences.zipWithIndex
      voterPreference.foreach { case (c1, i1) => {
        voterPreference.foreach { case (c2, i2) => {
          if (i1 < i2) {
            candidatePairKey(c1) = c2
            tallyTable(candidatePairKey) = tallyTable.getOrElse(candidatePairKey, rational0) + b.weight.numerator.toInt
            candidatePairKey.clear()
          }
        }
        }
      }
      }
    }

    def isWinner(x: Candidate, y: Candidate): Boolean = {
      val candidatePairKey = new MMap[Candidate, Candidate]
      candidatePairKey(x) = y
      if (tallyTable.get(candidatePairKey).get > majorityThreshold * getTotalVoters(election)) true else false

    }

    def tallyValue(x: Candidate, y: Candidate): Rational = {
      val pairKey = new MMap[Candidate, Candidate]
      pairKey(x) = y
      tallyTable.get(pairKey).get
    }

    ccandidates.foreach(x => {
      winnerMap(x) = rational0
      ccandidates.filter(y => (x != y) && isWinner(y, x)).foreach(z => {
        if (tallyValue(z, x) > winnerMap.getOrElse(x, 0)) {
          winnerMap.update(x, tallyValue(z, x))
        }
      })
    })

    winnerMap.toList.sortWith(_._2 < _._2).take(numVacancies)
  }
}