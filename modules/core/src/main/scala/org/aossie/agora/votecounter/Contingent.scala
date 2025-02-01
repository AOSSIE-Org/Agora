package org.aossie.agora.votecounter

import org.aossie.agora.model._

import scala.collection.mutable.{HashMap => MMap}

import spire.math.Rational

/** https://en.wikipedia.org/wiki/Contingent_vote */
object Contingent extends VoteCounter[PreferenceBallot] {

  val majorityThreshold = Rational(1, 2)

  override def winners[C <: Candidate](
      election: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {
    val tls                           = election.firstVotes(ccandidates)
    val ctSorted: List[(C, Rational)] = tls.toList.sortWith(_._2 > _._2)
    if (ctSorted.head._2 > majorityThreshold * election.length) {
      List(ctSorted.head)
    } else {
      val tlsSecondRound = ctSorted.take(2)
      val ccands: List[C] =
        ccandidates.filterNot(m => m != tlsSecondRound.head._1 && m != tlsSecondRound.tail.head._1)
      val secondRoundScores = new MMap[C, Rational]
      for (b <- election if !b.preferences.isEmpty) {
        val preferredCandidate = b.preferences.find(ccands.contains(_))
        preferredCandidate match {
          case Some(c) => secondRoundScores(c) = b.weight + (secondRoundScores.getOrElse(c, 0))
          case _       =>
        }
      }
      List(secondRoundScores.toList.sortWith(_._2 > _._2).head)
    }
  }

}
