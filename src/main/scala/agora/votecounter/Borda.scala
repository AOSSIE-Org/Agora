package agora.votecounter

import com.typesafe.scalalogging.LazyLogging
import agora.model.{Candidate, Election}
import agora.model.{PreferenceBallot => Ballot}

import scala.collection.mutable.{HashMap => Map}
import spire.math.Rational

/**
  * Created by deepeshpandey on 07/03/17.
  */
object Borda extends VoteCounter[Ballot] with LazyLogging{

  def totals(election: Election[Ballot], candidates: List[Candidate]): Map[Candidate, Rational] = {
    val m = new Map[Candidate, Rational]
    val numCandidates = candidates.length

    for (b <- election if !b.preferences.isEmpty) {

      // need to take the size of the list first and then calculate the borda scores
     b.preferences.zipWithIndex.foreach(preference => {
       m(preference._1) = m.getOrElse(preference._1, Rational(0,1)) + ((numCandidates - 1 - preference._2) * b.weight.numerator.toInt)
     })

    }
    m
  }

  def winners(election: Election[Ballot], ccandidates: List[Candidate], numVacancies: Int ):
  List[(Candidate,Rational)] = {


    logger.info("Borda rule computation called")

    val tls = totals(election, ccandidates)

    tls.toList.sortWith(_._2 > _._2).take(numVacancies)

  }

}
