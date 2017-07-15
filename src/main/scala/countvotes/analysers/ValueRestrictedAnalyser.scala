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
  def analyse(election: Election[WeightedBallot], ccandidates: List[Candidate]): Boolean = {

    // essentially try to find the triplet for which it fails otherwise just print on terminal
    val tripletlist = for (i <- ccandidates.indices;
                           j <- i + 1 until ccandidates.length;
                           k <- j + 1 until ccandidates.length)
      yield List(ccandidates(i), ccandidates(j), ccandidates(k))


    val failingtriplet = tripletlist.takeWhile(triplet => !valueRestrictedTriplet(triplet, election)).take(1)

    if (failingtriplet.isEmpty) {
      println("\n\nGiven election data satisfies value-restricted preferences.\n\n")
      true
    } else {
      println("\n\nGiven election data does not satisfies value-restricted preferences for the following triplet:\n")
      println(failingtriplet.flatten.mkString("\n"))
      false
    }

  }

  /**
    * marks the rankings for each candidate of the triplet and check's for the value restricted condition
    * Assumption: triplet = List[A, B, C] as per the value restricted notation from the link
    *
    * @param triplet  a triplet of candidate against which we want to check value restructed preferences
    * @param election election file
    */
  def valueRestrictedTriplet(triplet: List[Candidate], election: Election[WeightedBallot]): Boolean = {

    val tripletRankings = Array(Array(0,0,0), Array(0,0,0), Array(0,0,0))

    for (b <- election) {
      b.preferences.filter(c => triplet.contains(c)).zipWithIndex.foreach(c => {
        tripletRankings {triplet.indexOf(c._1)} {c._2} = 1
      })
    }

    // candidate i has never been ranked j in the entire election
    val hasAZeroEntry = for (i <- 0 until 3; j <- 0 until 3  if tripletRankings{i}{j} == 0) yield (i,j)

    hasAZeroEntry.size > 0


  }
}
