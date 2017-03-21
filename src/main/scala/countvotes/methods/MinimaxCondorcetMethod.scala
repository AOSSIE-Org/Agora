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
    val candidatePairKey = new MMap[Candidate, Candidate]
    val candidates = ccandidates.zipWithIndex

    // initialise the tally table
    candidates.foreach { case (c1, i1) => {
      candidates.foreach { case (c2, i2) => {
        if (i2 != i1) {

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
      val voterPreference = b.preferences
      voterPreference.zipWithIndex.foreach(preference => {

        voterPreference.zipWithIndex.foreach(candidate => {

          if (preference._2 < candidate._2) {
            candidatePairKey.put(preference._1, candidate._1)
            tallyTable.put(candidatePairKey, tallyTable.get(candidatePairKey).get + b.weight.numerator.toInt)
            candidatePairKey.clear()
          }

        })
      })
    }

    var c1WorstDefeat = rational0
    val listBuffer = new ListBuffer[(Candidate, Rational)]
    val totalVoters = election.length

    // find the worst pair wise defeat for each pair and find the minimum one
    candidates.foreach { case (c1, i1) => {

      c1WorstDefeat = rational0
      candidates.filter { case (c2, i2) => c2 != c1 } foreach { case (c3, i3) => {

        candidatePairKey.put(c3, c1)
        val c3c1ElectionResult = tallyTable.get(candidatePairKey).get

        if ((c3c1ElectionResult > majorityThreshold * totalVoters) && (c3c1ElectionResult > c1WorstDefeat)) {
          c1WorstDefeat = c3c1ElectionResult
        }
        candidatePairKey.clear()
      }
      }
      listBuffer.insert(i1, (c1, c1WorstDefeat))
    }
    }
    listBuffer.sortWith(_._2 < _._2).take(numVacancies).toList
  }


}
