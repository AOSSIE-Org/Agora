package countvotes.methods

import countvotes.structures._
import countvotes.structures.{Candidate, Input, Rational, Report, _}
import countvotes.structures.{Candidate, Rational, _}
import collection.mutable.{HashMap => Map}

/**
  * Created by deepeshpandey on 10/03/17.
  */
object KemenyYoungMethod extends VoteCountingMethod[WeightedBallot] {

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]

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

    var tallyTable = new Map[Map[Candidate, Candidate], Integer]
    var candidatePairKey = new Map[Candidate, Candidate]

    // initialise the tally table
    ccandidates.zipWithIndex.foreach(candidate => {
      ccandidates.zipWithIndex.foreach(preference => {
        if (preference._2 != candidate._2) {

          var candidatePair = new Map[Candidate, Candidate]
          candidatePair.put(candidate._1, preference._1)
          tallyTable.put(candidatePair, 0)
        }
      })

    })

    // update the tally table using election
    for (b <- election if !b.preferences.isEmpty) {
      val voterPreference = b.preferences
      b.preferences.zipWithIndex.foreach(preference => {

        voterPreference.zipWithIndex.foreach(candidate => {

          if (preference._2 < candidate._2) {
            candidatePairKey.put(preference._1, candidate._1)
            tallyTable.put(candidatePairKey, tallyTable.get(candidatePairKey).get + b.weight.numerator.toInt)
            candidatePairKey.clear()
          }

        })
      })
    }

    // permute the list and check for the maximum kemeny ranking

    var maxRankingScore = 0
    var maxRanking = new Array[Candidate](ccandidates.length)

    ccandidates.permutations.toList.foreach(ranking => {

      var currentRankingScore = 0
      ranking.zipWithIndex.foreach(candidate => {

        ranking.zipWithIndex.foreach(preference => {

          if (candidate._2 < preference._2) {
            var candidatePairKey = new Map[Candidate, Candidate]
            candidatePairKey.put(candidate._1, preference._1)
            currentRankingScore = currentRankingScore + tallyTable.get(candidatePairKey).get

          }
        })
      })
      // keep track of the maximum score and rankings
      if (currentRankingScore > maxRankingScore) {
        maxRankingScore = currentRankingScore
        maxRanking = ranking.toArray
      }
    })

    // return the final kemeny-Young rankings
    var kemenyYoungRanking = new Array[(Candidate, Rational)](ccandidates.length)

    maxRanking.toList.zipWithIndex.foreach(candidate => {

      kemenyYoungRanking.update(candidate._2, (candidate._1, new Rational(0,1)))
    })

    kemenyYoungRanking.toList
  }

}
