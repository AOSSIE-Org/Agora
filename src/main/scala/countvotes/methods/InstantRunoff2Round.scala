package countvotes.methods

import countvotes.structures._

import scala.collection.mutable.{HashMap => Map}

/**
  * Created by deepeshpandey on 03/06/17.
  * Algorithm : https://en.wikipedia.org/wiki/Two-round_system
  * Comments : It's a hybrid algorithm combining Instant Run-Off and 2-round voting. Two round voting cannot be handled exclusively
  *            becuase of its nature of having two rounds at different times
  * Assumptions : voters vote only once with preferential ballots
  * Rounds : only 2 as per 2-round voting
  */
object InstantRunoff2Round extends VoteCounter[Ballot] {

  override def winners(election: Election[Ballot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    val majorityRational = Rational(1, 2)
    val rnd1Winners = totals(election, ccandidates).toList.sortWith(_._2 > _._2).take(2)
    val totalVoters = Election.totalWeightedVoters(election)

    if (rnd1Winners.head._2 > majorityRational * totalVoters)
      List(rnd1Winners.head)
    else
      getSecondRoundWinner(election, ccandidates, rnd1Winners.map(c => c._1), totalVoters.toInt, numVacancies)

  }

  def getSecondRoundWinner(election: Election[Ballot], ccandidates: List[Candidate],
                           rnd1Winners: List[Candidate], totalVoters: Int, numVacancies: Int): List[(Candidate, Rational)] = {

    val m = new Map[Candidate, Rational]

    for (c <- ccandidates) m(c) = 0

    for (b <- election if b.preferences.nonEmpty) {
      val candidate = b.preferences.filter(c => rnd1Winners.contains(c)).take(1)
      m(candidate.head) = b.weight + m.getOrElse(candidate.head, 0)
    }

    m.toList.sortWith(_._2 > _._2).take(numVacancies)
  }
}
