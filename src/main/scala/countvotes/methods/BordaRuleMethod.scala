package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures.{Candidate, Rational, _}

import scala.collection.mutable.{HashMap => Map}

/**
  * Created by deepeshpandey on 07/03/17.
  */
object BordaRuleMethod extends Scrutiny[WeightedBallot] with LazyLogging{

  override def totals(election: Election[WeightedBallot], candidates: List[Candidate]): Map[Candidate, Rational] = {
    val m = new Map[Candidate, Rational]
    val totalCandidates = getCandidates(election).length

    for (b <- election if !b.preferences.isEmpty) {

      // need to take the size of the list first and then calculate the borda scores
     b.preferences.zipWithIndex.foreach(preference => {
       m(preference._1) = m.getOrElse(preference._1, new Rational(0,1)) + ((totalCandidates - 1 - preference._2) * b.weight.numerator.toInt)
     })

    }
    m
  }

  def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int ):
  List[(Candidate,Rational)] = {


    logger.info("Borda rule computation called")

    val tls = totals(election, ccandidates)

    tls.toList.sortWith(_._2 > _._2).take(numVacancies)

  }

}
