package org.aossie.agora.votecounter

import com.typesafe.scalalogging.LazyLogging
import spire.math.Rational

import org.aossie.agora.model._
import org.aossie.agora.model.{PreferenceBallot => Ballot}


import scala.collection.mutable.{HashMap => MMap}

/**
  * Algorithm : https://en.wikipedia.org/wiki/Coombs%27_method
  * Note: This voting method requires voters to rank all the candidates
  */
object Coomb extends VoteCounter[Ballot] with LazyLogging{

  private val majorityThreshold = Rational(1,2)

  def winners(election: Election[Ballot], ccandidates: List[Candidate], numVacancies: Int ):
  List[(Candidate,Rational)] = {

    logger.info("computing coomb winner")

    val firstRankedMap = new MMap[Candidate, Rational]

    // check if there is a majority winner

    for (b <- election if b.preferences.nonEmpty) {

        b.preferences.find(c => ccandidates.contains(c)) match {
        case Some(candidate) => firstRankedMap(candidate) = firstRankedMap.getOrElse(candidate, Rational(0, 1)) + b.weight
        case None => {}
      }
    }

    if(firstRankedMap.maxBy(_._2)._2 > Rational(1, 2) * election.weight) {

      List(firstRankedMap.maxBy(_._2))

    } else {
      // winner not found create the last ranked map and filter the highest last ranked candidate

      val lastRankedMap = new MMap[Candidate, Rational]
      for (b <- election if b.preferences.nonEmpty) {

          assert(b.preferences.find(c => ccandidates.contains(c)) !=  b.preferences.reverseIterator.find(c => ccandidates.contains(c)))
          b.preferences.reverseIterator.find(c => ccandidates.contains(c)) match {
          case Some(candidate) => lastRankedMap(candidate) = lastRankedMap.getOrElse(candidate, Rational(0, 1)) + b.weight
          case None => {}
        }

      }

      winners(election, ccandidates.filter {_ != lastRankedMap.maxBy(_._2)._1}, numVacancies)
    }

  }

}
