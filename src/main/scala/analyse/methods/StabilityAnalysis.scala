package analyse.methods

import countvotes.structures.Candidate

abstract class StabilityAnalysis {

  // generate election profiles for 3 candidates and 3 voters to check the stability of algorithms
  def preferenceProfiles(nCandidate: Int, nVoters: Int): List[List[List[Candidate]]] = {

    val candidatePermutations = ('A' to 'Z').toList.take(nCandidate).map(c => Candidate(c.toString)).permutations.toList

    val voterProfiles = List.fill(nVoters)(0 until nCandidate).flatten.combinations(nVoters)

    voterProfiles.map(combination => {
      combination.map(num => candidatePermutations(num))
    }).toList
  }

  // calculate the kendall tau distance between two profiles
  def kendallTauDistance(profile: List[List[List[Candidate]]]): Int = {

    var kTDistance = 0

    profile(0) zip profile(1) foreach(pair => {

      // calculate the kendall tau distance between pair._1 and pair._
      pair._1.foreach(c1 => {
        pair._1.foreach(c2 => {
          if ((pair._1.indexOf(c1) < pair._1.indexOf(c2)) && (pair._2.indexOf(c1) > pair._2.indexOf(c2))) {
            kTDistance += 1
          }
        })})})

    kTDistance
  }

  def analyse(): Unit

}
