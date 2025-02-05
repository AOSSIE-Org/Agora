package org.aossie.agora.votecounter

import org.aossie.agora.model._

import scala.collection.mutable.{HashMap => Map}

import spire.math.Rational

/** Created by deepeshpandey on 03/06/17. Algorithm : https://en.wikipedia.org/wiki/Two-round_system
  * Comments : It's a hybrid algorithm combining Instant Run-Off and 2-round voting. Two round
  * voting cannot be handled exclusively becuase of its nature of having two rounds at different
  * times Assumptions : voters vote only once with preferential ballots Rounds : only 2 as per
  * 2-round voting
  */
object InstantRunoff2Round extends VoteCounter[PreferenceBallot] {

  override def winners[C <: Candidate](
      election: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {

    val majorityRational = Rational(1, 2)
    val rnd1Winners      = election.firstVotes(ccandidates).toList.sortWith(_._2 > _._2).take(2)
    val totalVoters      = election.weight

    if (rnd1Winners.head._2 > majorityRational * totalVoters)
      List(rnd1Winners.head)
    else
      getSecondRoundWinner(
        election,
        ccandidates,
        rnd1Winners.map(c => c._1),
        totalVoters.toInt,
        numVacancies
      )

  }

  def getSecondRoundWinner[C <: Candidate](
      election: Election[C, PreferenceBallot],
      ccandidates: List[C],
      rnd1Winners: List[C],
      totalVoters: Int,
      numVacancies: Int
  ): List[(C, Rational)] = {

    val m = new Map[C, Rational]

    for (c <- ccandidates) m(c) = 0

    for (b <- election if b.preferences.nonEmpty) {
      val candidate = b.preferences.filter(c => rnd1Winners.contains(c)).take(1)
      m(candidate.head) = b.weight + m.getOrElse(candidate.head, 0)
    }

    m.toList.sortWith(_._2 > _._2).take(numVacancies)
  }

}
