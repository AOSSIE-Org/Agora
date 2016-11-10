package countvotes.methods

import countvotes.structures._
import countvotes.algorithms._
import scala.math._

object EgalitarianNaiveImplementation extends EgalitarianVotingMethod[WeightedBallot] {
  var allCandidates: List[Candidate] = List.empty

  def runScrutiny(election: Election[WeightedBallot], numVacancies: Int):  Report[WeightedBallot] = {
    allCandidates = getCandidateList(election)
    println("Number of WeightedBallots: " + election.length)
    report.setWinners(computeWinners(election, numVacancies))
    report
  }

  override def computeWinners(election: Election[WeightedBallot], numVacancies: Int): List[(Candidate,Rational)] = {
    //The candidate list wouldn't have to be determined this way (would be supplied from the data)
    val candidateCount: Int = allCandidates.length
    val candidateSubsets: List[List[Candidate]] = getCandidateSubsets(allCandidates,candidateCount,List.empty,0,numVacancies)

    if(candidateCount < numVacancies) {println("not enough candidates") }

    var currentMax: Double = 0
    var currentCandidates: List[Candidate] = List.empty

    for(i <- candidateSubsets){
      var contemplatedMax: Double = 0
      contemplatedMax = socialWelfare(election, i)
      if(currentMax < contemplatedMax){
        currentMax = contemplatedMax
        currentCandidates = i
      }
    }

    var currentCandidatesForReturn : List[(Candidate,Rational)] = List.empty
    for(i <- currentCandidates){
      currentCandidatesForReturn = currentCandidatesForReturn :+ (i,new Rational(n=1,d=1))
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
        combinations = combinations ::: getCandidateSubsets(candidates, candidateCount, stub :+ candidates(i),i+1,remaining-1)
      }
    }
    combinations
  }
}
