package analyse.methods

import countvotes.methods.BordaRuleMethod
import countvotes.structures.{Candidate, WeightedBallot}

object BordaStability extends StabilityAnalysis {


  // try to find the minimum Kendall tau distance for which the method outputs two different winners
  override def analyse(): Unit = {

    val candidates = ('A' to 'Z').toList.take(3).map(c => Candidate(c.toString))

    val minKendallTauDistances = preferenceProfiles(3, 3)
      .combinations(2)
      .filter(profile => haveDifferentWinnter(profile(0), profile(1), candidates))
      .map(profile => (kendallTauDistance(profile), profile(0), profile(1)))
      .toList

    if (minKendallTauDistances.isEmpty) {
      // this should happen only for trivial, random and bizarre voting rules
      println("\n\n\n\nBorda voting rule is fully stable for all 3 candidates and 3 voters profiles\n\n\n\n")
    } else {

      val minKendallTauProfiles = minKendallTauDistances.minBy(_._1)
      println(s"\n\n\n\n Borda is unstable for a Kendall Tau distance of ${minKendallTauProfiles._1} for a 3 candidates and 3 voters.\n\n\n\n")
      println("Preference profiles for this kendall tau distances are: \n\n")
      println(minKendallTauProfiles._2.mkString("\n"))
      println("\n\n")
      println(minKendallTauProfiles._3.mkString("\n"))

    }
  }

  def haveDifferentWinnter(profile1: List[List[Candidate]], profile2: List[List[Candidate]], candidates: List[Candidate]): Boolean = {

    val preferenceProfile1 = profile1.map(profile => WeightedBallot(profile, 1, 1))
    val preferenceProfile2 = profile2.map(profile => WeightedBallot(profile, 1, 1))

    BordaRuleMethod.winners(preferenceProfile1, candidates, 1) != BordaRuleMethod.winners(preferenceProfile2, candidates, 1)
  }


}
