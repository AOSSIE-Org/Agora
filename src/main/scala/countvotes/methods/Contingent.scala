package countvotes.methods


import countvotes.structures._

import scala.collection.mutable.{HashMap => MMap}

/**
  * https://en.wikipedia.org/wiki/Contingent_vote
  */
object Contingent extends VoteCounter[Ballot] {

  val majorityThreshold = Rational(1, 2)

  override def winners(election: Election[Ballot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {
    val tls = totals(election, ccandidates)
    val ctSorted: List[(Candidate, Rational)] = tls.toList.sortWith(_._2 > _._2)
    if (ctSorted.head._2 > majorityThreshold * election.length) {
      List(ctSorted.head)
    }
    else {
      val tlsSecondRound = ctSorted.take(2)
      val ccands: List[Candidate] = ccandidates.filterNot(m => m!=tlsSecondRound.head._1 && m!=tlsSecondRound.tail.head._1)
      val secondRoundScores = new MMap[Candidate, Rational]
      for(b<-election if !b.preferences.isEmpty) {
        val preferredCandidate = b.preferences.find(ccands.contains(_))
        preferredCandidate match {
          case Some(c) => secondRoundScores(c) = b.weight + (secondRoundScores.getOrElse(c, 0))
          case _ =>
      }
      }
      List(secondRoundScores.toList.sortWith(_._2 > _._2).head)
    }
  }

}