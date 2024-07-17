package org.aossie.agora.votecounter

import org.aossie.agora.model._
import org.aossie.agora.votecounter.common.PreferencePairwiseComparison

import spire.math.Rational

/** Link: https://drive.google.com/file/d/0B4uPp6wWiMpSMHZqaWVva0RZZjA/view?usp=sharing Important:
  * Dominion of a candidate a is D(a) = { b ∈ A : a >M b }, Dominators of candidate a is D'(a) = { b
  * ∈ A : b >M a } Every tournament is zero-sum multiplayer game and admits a unique probability
  * distribution - due to Von Neumann's Minimax theorem(https://www.youtube.com/watch?v=toP9XPT7Bv4)
  * Bipartisan Set is defined as : BP(A,PM) = {x∈A : p(x)>0, p balanced for (A,PM)} where PM is
  * majority graph
  */
object BipartisanSet
    extends VoteCounter[Candidate, PreferenceBallot]
    with PreferencePairwiseComparison[Candidate, PreferenceBallot] {

  def runVoteCounter(
      election: Election[Candidate, PreferenceBallot],
      candidates: List[Candidate],
      param: Parameters
  ): Report[Candidate, PreferenceBallot] = {

    // print("\n INPUT ELECTION: \n")
    // //printElection(election)

    val result: Result[Candidate]                   = new Result
    val report: Report[Candidate, PreferenceBallot] = new Report[Candidate, PreferenceBallot]

    report.setCandidates(candidates)

    report.setWinners(bipartisanSet(election, candidates, param))

    report
  }

  // scalastyle:off method.length
  def bipartisanSet(
      election: Election[Candidate, PreferenceBallot],
      candidates: List[Candidate],
      parameters: Parameters
  ): List[(Candidate, Rational)] = {

    // check if the probability distribution is given for each candidate
    require(
      parameters.probabilityDistribution.isDefined && parameters.probabilityDistribution.get.length == candidates.length,
      "inconsistency in candidates and probability distribution"
    )

    val distribution = parameters.probabilityDistribution.get.reduce(_ ++ _)

    // check if the name given in the distribution is consistent with the candidates names
    require(
      distribution.forall { case (cand, prob) =>
        candidates.exists(candidate => candidate.name == cand)
      },
      "inconsistencies in candidates names"
    )

    val candidatesProbabilities = candidates.map(cand => (cand, distribution(cand.name)))

    val majorityMatrix = pairwiseComparison(election, candidates)

    // Dominion of a candidate a is D(a) = { b ∈ A : a >M b }
    def dominions(candidate: Candidate): List[(Candidate, Double)] = {
      candidatesProbabilities.filter { case (cand, prob) =>
        cand != candidate &&
        majorityMatrix(candidates.indexOf(candidate))(candidates.indexOf(cand)) > majorityMatrix(
          candidates.indexOf(cand)
        )(candidates.indexOf(candidate))
      }
    }

    // Dominators of candidate a is D'(a) = { b ∈ A : b >M a }
    def dominators(candidate: Candidate): List[(Candidate, Double)] = {
      candidatesProbabilities.filter { case (cand, prob) =>
        cand != candidate &&
        majorityMatrix(candidates.indexOf(cand))(candidates.indexOf(candidate)) > majorityMatrix(
          candidates.indexOf(candidate)
        )(candidates.indexOf(cand))
      }
    }

    def probabilityMargin(
        dominions: List[(Candidate, Double)],
        dominators: List[(Candidate, Double)]
    ): Double = {
      dominions.map { case (cand, prob) => prob }.sum - dominators.map { case (cand, prob) =>
        prob
      }.sum
    }

    // p is balanced if (p(x)>0 ⇔ mp(x)=0) and (p(x)=0 ⇔ mp(x)<0)
    def balancedProbabilityDistribution(): Boolean = {
      candidatesProbabilities.forall { case (cand, prob) =>
        if (prob > 0) {
          probabilityMargin(dominions(cand), dominators(cand)) == 0
        } else if (prob == 0) {
          probabilityMargin(dominions(cand), dominators(cand)) < 0
        } else {
          false
        }
      }
    }

    // check if p is a balanced probability distribution => p is balanced if (p(x)>0 ⇔ mp(x)=0) and (p(x)=0 ⇔ mp(x)<0)
    require(balancedProbabilityDistribution, "probability distribution is not balanced!")

    // BP(A,PM) = {x∈A : p(x)>0, p balanced for (A,PM)} where PM is majority graph
    candidatesProbabilities.filter { case (cand, prob) =>
      prob > 0 &&
      probabilityMargin(dominions(cand), dominators(cand)) == 0
    }.map { case (cand, prob) => (cand, Rational(prob)) }
  }

  override def winners(
      e: Election[Candidate, PreferenceBallot],
      ccandidates: List[Candidate],
      numVacancies: Int
  ): List[(Candidate, Rational)] = ???

}
