package countvotes.analysers

import collection.mutable.{ListBuffer, HashMap => Map}
import countvotes.structures.{Candidate, Election, Rational, WeightedBallot}

/**
  * This analyser analyses for Sen's Value restricted preferences
  * source link : https://www.youtube.com/watch?v=F51U9Sv9QNo&t=26s
  */
object ValueRestrictedAnalyser extends PreferenceAnalysisMethod[WeightedBallot] {

  /**
    * assumption : voters preference relations are complete over set of candidates.
    * for every triplet of candidates check if any of value restricted conditions are true.
    * For any triplet(A, B, C) check if (a(ABC) = 0 or a(BCA) = 0 or a(CAB) = 0) and (a(CBA) = 0 or a(ACB) = 0 or a(BAC) = 0)
    * where a(CAB) is the total number of voters having preferences in order of C > A > B.
    *
    * @param election    => election file assuming preferences with ordering ">"
    * @param ccandidates => candidates list
    * @return
    */
  def analyse(election: Election[WeightedBallot], ccandidates: List[Candidate]): Unit = {


    require(election forall (b => b.preferences.length == ccandidates.length))

    // generate all the triplet of candidates
    val tripletlist = for (i <- ccandidates.indices;
                           j <- i + 1 until ccandidates.length;
                           k <- j + 1 until ccandidates.length)
      yield List(ccandidates(i), ccandidates(j), ccandidates(k))


    // try to find the failing triplet and print it out
    val failingTriplet = tripletlist.find(triplet => !triplet.exists(cand => {
      !election.exists(b => b.preferences.filter(p => triplet.contains(p)).indexOf(cand) == 0) ||
        !election.exists(b => b.preferences.filter(p => triplet.contains(p)).indexOf(cand) == 1) ||
        !election.exists(b => b.preferences.filter(p => triplet.contains(p)).indexOf(cand) == 2)
    }))

    failingTriplet match {
      case Some(list) => println("Preference profile is not value restricted for the triplet " + list.mkString(" , "))
      case None => println("Preference profile is value restricted")
    }

    // will compare the efficiency once a bigger preference profile is found
    /*val failingtriplet = tripletlist.find(triplet => !valueRestrictedTriplet(triplet, election))

        if (failingtriplet.isEmpty) {
          println("\n\nGiven election data satisfies value-restricted preferences.\n\n")
        } else {
          println("\n\nGiven election data does not satisfies value-restricted preferences for the following triplet:\n")
          println(failingtriplet.flatten.mkString("\n"))
        }*/
  }

  /**
    * marks the rankings for each candidate of the triplet and check's for the value restricted condition
    * Assumption: triplet = List[A, B, C] as per the value restricted notation from the link
    *
    * @param triplet  a triplet of candidate against which we want to check value restructed preferences
    * @param election election file
    */
  def valueRestrictedTriplet(triplet: List[Candidate], election: Election[WeightedBallot]): Boolean = {

    val tripletRankings = Array.ofDim[Int](3, 3)

    for (b <- election) {
      b.preferences.filter(triplet.contains(_)).zipWithIndex.foreach(c => {
        tripletRankings {triplet.indexOf(c._1)} {c._2} = 1
      })
    }

    tripletRankings.exists(p => p.contains(0))

  }
}
