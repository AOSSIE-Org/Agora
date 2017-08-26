package analyse

import countvotes.methods._
import countvotes.structures._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.language.postfixOps
import scala.util.Random


object StabilityAnalyser {


  def main(args: Array[String]): Unit = {

    val report: Report[WeightedBallot] = new Report[WeightedBallot]

    val (candidates, elections) = generateElections(20, 100, 5)

    val electionsPairs = elections.combinations(2).filter(kendallTauDistance(_) != 0) toList

    val analysisArray = new ArrayBuffer[(String, Double, Double)]

    analysisArray.append(analyseStability(BordaRuleMethod, electionsPairs, candidates))
    //analyseStability(MajorityRuleMethod, electionsPairs, candidates) /*some cases there might be no majority winner*/
    analysisArray.append(analyseStability(ApprovalRule, electionsPairs, candidates))
    analysisArray.append(analyseStability(KemenyYoungMethod, electionsPairs, candidates))
    analysisArray.append(analyseStability(BaldwinMethod, electionsPairs, candidates))
    analysisArray.append(analyseStability(NansonMethod, electionsPairs, candidates))
    analysisArray.append(analyseStability(InstantRunoff2Round, electionsPairs, candidates))
    analysisArray.append(analyseStability(CoombRuleMethod, electionsPairs, candidates))
    analysisArray.append(analyseStability(InstantExhaustiveBallot, electionsPairs, candidates))
    analysisArray.append(analyseStability(ContingentMethod, electionsPairs, candidates))
    analysisArray.append(analyseStability(MinimaxCondorcetMethod, electionsPairs, candidates))
    analysisArray.append(analyseStability(CopelandMethod, electionsPairs, candidates))
    //analyseStability(UncoveredSetMethod, electionsPairs, candidates) /* in some cases the uncovered set could be empty*/
    analysisArray.append(analyseStability(SmithSetMethod, electionsPairs, candidates))
    analysisArray.append(analyseStability(InstantExhaustiveDropOffRule, electionsPairs, candidates))
    analysisArray.append(analyseStability(PreferentialBlockVoting, electionsPairs, candidates))
    analysisArray.append(analyseStability(HybridPluralityPreferentialBlockVoting, electionsPairs, candidates))
    //analyseStability(OklahomaMethod, electionsPairs, candidates) /* going into infinite loop for some random election*/
    analysisArray.append(analyseStability(SequentialProportionalApprovalVoting, electionsPairs, candidates))
    analysisArray.append(analyseStability(ProportionalApprovalVoting, electionsPairs, candidates))
    analysisArray.append(analyseStability(SatisfactionApprovalVoting, electionsPairs, candidates))

    report.setStabilityAnalysis(analysisArray.toList)

    report.writeStabilityAnalysis("../Agora/files/Examples/analysis/" + "stabilityanalysis.txt")

  }


  def analyseStability(vcm: VoteCountingMethod[WeightedBallot], electionsPair: List[List[Election[WeightedBallot]]],
                       candidates: List[Candidate]): (String, Double, Double) = {


    val electionResults = electionsPair.map(stability(vcm, _, candidates))

    val averageRatio = electionResults.sum / electionResults.length

    val varianceRatio = electionResults.map(c => Math.pow((c - averageRatio), 2)).sum / electionResults.length

    (vcm.getClass.getSimpleName, averageRatio, varianceRatio)

  }

  def stability(vcm: VoteCountingMethod[WeightedBallot], elections: List[Election[WeightedBallot]], candidates: List[Candidate]): Double = {

    val winnerEA = vcm.winners(elections(0), candidates, 1)
    val winnerEB = vcm.winners(elections(1), candidates, 1)
    val winnersDistance = winnerSetComparison(winnerEA map {_._1}, winnerEB map {_._1})
    val ktDistance = kendallTauDistance(elections)

    winnersDistance.toDouble / ktDistance.toDouble
  }

  // generate n random election of m voters and c candidates
  def generateElections(n: Int, m: Int, c: Int): (List[Candidate], List[Election[WeightedBallot]]) = {

    require(c < 26)

    val candidates = ('A' to 'Z') take c map(name => new Candidate(name.toString)) toList

    val election = for {
      i <- List.range(1, n)
    } yield {
      for {
        j <- List.range(1, m)
      } yield WeightedBallot(Random.shuffle(candidates), i, 1)
    }

    (candidates,election)
  }

  // calculate the kendall tau distance between two profiles
  def kendallTauDistance(profile: List[Election[WeightedBallot]]): Int = {

    var kTDistance = 0

    profile(0) zip profile(1) foreach { case (we1, we2) => {
      we1.preferences.foreach(c1 => {
        we1.preferences.foreach(c2 => {
          if ((we1.preferences.indexOf(c1) < we1.preferences.indexOf(c2))
            && (we2.preferences.indexOf(c1) > we2.preferences.indexOf(c2))) {
            kTDistance += 1
          }})})}
    }

    kTDistance
  }

  // http://www.nature.com/nature/journal/v234/n5323/abs/234034a0.html?foxtrotcallback=true
  def winnerSetComparison(winnerEA: List[Candidate], winnerEB: List[Candidate]): Rational = {

    if ((winnerEA union winnerEB).distinct.nonEmpty) {
      ((winnerEA intersect winnerEB).distinct length) / (winnerEA union winnerEB).distinct.length

    } else {
      Rational(0, 1)
    }
  }

}
