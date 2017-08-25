package analyse.methods

import countvotes.methods.BordaRuleMethod
import countvotes.structures.{Candidate, Election, Rational, WeightedBallot}

object BordaStability extends StabilityAnalysis {


  def analyse(): Unit = {

    val candidates = ('A' to 'Z') map (name => new Candidate(name.toString)) take 5 toList

    val elections = generateElections(20, 100, 5)

    val electionsData = elections.combinations(2).map(stability(_, candidates)).toList

    val ratioAverage = electionsData.sum / electionsData.length

    val varianceRatio = electionsData.map(c => Math.pow((c - ratioAverage), 2)).sum / electionsData.length

    println("\n\nResults of Borda Stability analysis\n\n")

    println("For 20 random elections from 100 voters for 5 candidates")

    println("Stability Average  = " + ratioAverage)

    println("Stability Variance  = " + varianceRatio)

  }

  def stability(elections: List[Election[WeightedBallot]], candidates: List[Candidate]): Double = {

    val winnerEA = BordaRuleMethod.winners(elections(0), candidates, 1)
    val winnerEB = BordaRuleMethod.winners(elections(1), candidates, 1)
    val winnersDistance = winnerSetComparison(winnerEA map{_._1}, winnerEB map{_._1})
    val ktDistance = kendallTauDistance(elections)

    if (ktDistance == 0) 0.0 else (winnersDistance/ktDistance).toDouble


  }



}
