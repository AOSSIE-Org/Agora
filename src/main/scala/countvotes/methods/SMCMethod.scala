package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures.{Weight, WeightedBallot}
import countvotes.structures._


/**
  * Created by deepeshpandey on 03/06/17.
  * About : Sequential Majority Comparison (SMC): Fix some enumeration {x1, x2, . . . , xm} of the
  * alternatives. The winner of round 1 is x1; the winner of round i + 1 is the winner w of
  * round i, if w >(majority) xi+1, and is xi+1, if xi+1 >(majority) w; and the ultimate winner is the winner
  * of round m.
  */
object SMCMethod extends VoteCountingMethod[WeightedBallot] with LazyLogging {

  private val report: Report[WeightedBallot] = new Report[WeightedBallot]

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], param: Parameters, numVacancies: Int): Report[WeightedBallot] = {

    print("\n INPUT ELECTION: \n")
    printElection(election)

    report.setCandidates(candidates)

    report.setWinners(smcWinner(election, candidates, param, numVacancies))

    report
  }

  def smcWinner(election: Election[WeightedBallot], ccandidates: List[Candidate], param: Parameters, numVacancies: Int):
  List[(Candidate, Rational)] = {

    // it may be possible that param candidates and actual candidates are inconsistent
    require(param.comparisonOrder.isDefined && param.comparisonOrder.get.forall(c => ccandidates.exists(cand => cand.name == c)))

    val zeroRational = Rational(0, 1)
    val majorityRational = Rational(1, 2)

    val totalVoters = Election.totalWeightedVoters(election)
    val electionResponse = getPairwiseComparisonForWeightedElection(election, ccandidates)

    // generate the ordered list of candidates
    val candOrderList = param.comparisonOrder.get.map(name => ccandidates.find(cand => cand.name == name).get)

    List((candOrderList.head /: candOrderList.tail)((cA, cB) => {
      if (electionResponse(ccandidates.indexOf(cA))(ccandidates.indexOf(cB)) > majorityRational * totalVoters) cA else cB
    })).map(c => (c, zeroRational))

  }

  override def winners(e: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = ???
}