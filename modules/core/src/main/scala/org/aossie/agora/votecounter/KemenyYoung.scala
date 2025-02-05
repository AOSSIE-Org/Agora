package org.aossie.agora.votecounter

import org.aossie.agora.model._

import scala.collection.mutable.{HashMap => Map}

import spire.math.Rational

/** Created by deepeshpandey on 10/03/17. */
object KemenyYoung extends VoteCounter[PreferenceBallot] {

  private val rationalZero = Rational(0, 1)

  // scalastyle:off cyclomatic.complexity
  // scalastyle:off method.length
  def winners[C <: Candidate](
      election: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {

    var tallyTable       = new Map[Map[C, C], Integer]
    var candidatePairKey = new Map[C, C]
    val candidates       = ccandidates.zipWithIndex

    // initialise the tally table
    candidates.foreach { case (c1, i1) =>
      candidates.foreach { case (c2, i2) =>
        if (i2 != i1) {

          var candidatePair = new Map[C, C]
          candidatePair(c1) = c2
          tallyTable(candidatePair) = 0
        }
      }
    }

    // update the tally table using election
    for (b <- election if !b.preferences.isEmpty) {
      val voterPreference = b.preferences
      b.preferences.zipWithIndex.foreach(preference => {

        voterPreference.zipWithIndex.foreach(candidate => {

          if (preference._2 < candidate._2) {
            candidatePairKey.put(preference._1, candidate._1)
            tallyTable.put(
              candidatePairKey,
              tallyTable.get(candidatePairKey).get + b.weight.numerator.toInt
            )
            candidatePairKey.clear()
          }

        })
      })
    }

    // permute the list and check for the maximum kemeny ranking

    var maxRankingScore     = 0
    var maxRanking: List[C] = Nil

    ccandidates.permutations.toList.foreach { ranking =>
      var currentRankingScore = 0
      val currentRanking      = ranking.zipWithIndex

      currentRanking.foreach { case (c1, i1) =>
        currentRanking.foreach { case (c2, i2) =>
          if (i1 < i2) {
            var candidatePairKey = new Map[C, C]
            candidatePairKey.put(c1, c2)
            currentRankingScore = currentRankingScore + tallyTable.get(candidatePairKey).get
          }
        }
      }
      // keep track of the maximum score and rankings

      if (currentRankingScore > maxRankingScore) {
        maxRankingScore = currentRankingScore
        maxRanking = ranking
      }
    }

    maxRanking.map(candidate => (candidate, rationalZero)).take(numVacancies)
  }

}
