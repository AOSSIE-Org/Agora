package agora.votecounter

import agora.model._
import agora.model.{PreferenceBallot => Ballot}

import spire.math.Rational

object EgalitarianBrute extends Egalitarian[Ballot] {

  def winners(election: Election[Ballot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate,Rational)] = {
    val candidateCount: Int = ccandidates.length

    val candidateSubsets: List[List[Candidate]] = getCandidateSubsets(ccandidates,candidateCount,List.empty,0,numVacancies)

    if(candidateCount < numVacancies) {println("not enough candidates") }

    var currentMax: Double = 0
    var currentCandidates: List[Candidate] = List.empty

    for(i <- candidateSubsets){
      var contemplatedMax: Double = 0
      contemplatedMax = socialWelfare(election, i, candidateCount)
      if(currentMax < contemplatedMax){
        currentMax = contemplatedMax
        currentCandidates = i
      }
    }
    var currentCandidatesForReturn : List[(Candidate,Rational)] = List.empty
    for(i <- currentCandidates){
      currentCandidatesForReturn = currentCandidatesForReturn :+ (i, Rational(n=1,d=1))
    }
    currentCandidatesForReturn
  }

  def getCandidateSubsets(candidates: List[Candidate], candidateCount: Int, stub: List[Candidate], location: Int, remaining: Int): List[List[Candidate]] = {
    var combinations: List[List[Candidate]] = List.empty
    if(remaining == 1){
      for(i <- location to (candidateCount-1)){ //Kept in "old" system as different subsets are used, this seems the clearest way to do it
        var completedSubset: List[Candidate] = stub :+ candidates(i)
        combinations = combinations :+ completedSubset
      }
    }
    else{
      for(i <- location to (candidateCount-remaining)) {
        combinations = combinations ::: getCandidateSubsets(candidates, candidateCount, stub :+ candidates(i),i + 1, remaining - 1)
      }
    }
    combinations
  }
}
