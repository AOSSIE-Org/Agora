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
    *
    * @param election    => election file assuming preferences with ordering ">"
    * @param ccandidates => candidates list
    * @return
    */
  def analyse(election: Election[WeightedBallot], ccandidates: List[Candidate]): Unit = {

    // essentially try to find the triplet for which it fails otherwise just print on terminal
    val triplets = ccandidates.toSet[Candidate].subsets().filter(p => p.size == 3).map(_.toList).toList

    val failingtriplet = triplets.takeWhile(triplet => !valuerestrictedtriplet(triplet, election)).take(1)

    if (failingtriplet.isEmpty) {
      println("\n\nGiven election data satisfies value-restricted preferences\n\n")
    } else {
      println("\n\nGiven election data does not satisfies value-restricted preferences for the following triplet\n")
      println(failingtriplet.flatten.mkString("\n"))
    }

  }

  /**
    * calculates number of voters which specfic preferences and checks for the
    * Assumption: triplet = List[A, B, C] as per the value restricted notation from the link
    *
    * @param triplet  a triplet of candidate against which we want to check value restructed preferences
    * @param election election file
    */
  def valuerestrictedtriplet(triplet: List[Candidate], election: Election[WeightedBallot]): Boolean = {

    // generate permutations for the triplet and intialize them with zero
    var preferenceMap = Map() ++ triplet.permutations.toList.map(c => (c, Rational(0, 1)))

    for (b <- election if b.preferences.nonEmpty) {
      val votertripletorder = b.preferences.takeWhile(c => triplet.contains(c))
      preferenceMap(votertripletorder) = preferenceMap(votertripletorder) + b.weight
    }

    val keyABC = getCandidateOrderKey(List(1, 2, 3), triplet)
    val keyACB = getCandidateOrderKey(List(1, 3, 2), triplet)
    val keyBAC = getCandidateOrderKey(List(2, 1, 3), triplet)
    val keyBCA = getCandidateOrderKey(List(2, 3, 1), triplet)
    val keyCAB = getCandidateOrderKey(List(3, 1, 2), triplet)
    val keyCBA = getCandidateOrderKey(List(3, 2, 1), triplet)
    val zero = Rational(0, 1)

    // check value restricted preferences
    if (((preferenceMap(keyABC) == zero) || (preferenceMap(keyCAB) == zero) || (preferenceMap(keyBCA) == zero))
      && ((preferenceMap(keyCBA) == zero) || (preferenceMap(keyBAC) == zero) || (preferenceMap(keyACB) == zero))) {
      true
    } else {
      false
    }
  }


  /**
    * to check for the value restricted preferences we need keys for each of the 6 possible permutations for triplet size of 3
    * @param order permutation order of the triplet
    * @param ccandidates triplet in process
    * @return
    */
  def getCandidateOrderKey(order: List[Int], ccandidates: List[Candidate]): List[Candidate] = {

    val orderedCandidates = new ListBuffer[Candidate]

    orderedCandidates.insert(0, ccandidates(order(0) - 1))
    orderedCandidates.insert(1, ccandidates(order(1) - 1))
    orderedCandidates.insert(2, ccandidates(order(2) - 1))

    orderedCandidates.toList

  }


}
