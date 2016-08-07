package countvotes.methods

import countvotes.structures._
import countvotes.algorithms._
import scala.math._

object BrunoMethod extends BrunoMethodClass[ACTBallot] {

  def runScrutiny(election: Election[ACTBallot], numVacancies: Int):  Report[ACTBallot] = {
    println("Number of ballots: " + election.length)
    report.setWinners(computeWinners(election, numVacancies))
    report
  }

  def computeWinners(election: Election[ACTBallot], numVacancies: Int): List[(Candidate,Rational)] = {
    val fairness: Double = 1

    //The candidate list wouldn't have to be determined this way (would be supplied from the data)
    val candidateList: List[Candidate] = getCandidateList(election)
    val candidateCount: Int = candidateList.length
    val candidateSubsets: List[List[Candidate]] = getCandidateSubsets(candidateList,candidateCount,List.empty,0,numVacancies)

    if(candidateCount < numVacancies) {println("not enough candidates") }

    var currentMax: Double = 0
    var contemplatedMax: Double = 0
    var currentCandidates: List[Candidate] = List.empty

    for(i <- 0 to (candidateSubsets.length-1)){
      contemplatedMax = socialWelfare(election, candidateSubsets(i), fairness)
      if(currentMax < contemplatedMax){
        currentMax = contemplatedMax
        currentCandidates = candidateSubsets(i)
      }
    }

    //To fit with the current return system
    var currentCandidatesForReturn : List[(Candidate,Rational)] = List.empty
    for(i <- 0 to (currentCandidates.length - 1)){
      currentCandidatesForReturn = currentCandidatesForReturn :+ (currentCandidates(i),new Rational(n=1,d=1))
    }

    currentCandidatesForReturn
  }

  def getCandidateSubsets(candidates: List[Candidate], candidateCount: Int, stub: List[Candidate], location: Int, remaining: Int): List[List[Candidate]] = {
    var combinations: List[List[Candidate]] = List.empty

    if(remaining == 1){
      for(i <- location to (candidateCount-1)){
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

  def getCandidateList(election: Election[ACTBallot]): List[Candidate] = {
    var candidateList:List[Candidate] = List()
    for(i <- 0 to (election.length-1)){
      for(j <- 0 to (election(i).preferences.length-1)){
        if(!(candidateList contains election(i).preferences(j))){
          candidateList = candidateList :+ election(i).preferences(j)
        }
      }
    }
    candidateList
  }

  def rank(election: Election[ACTBallot], voter: Int, candidate: Candidate): (Boolean,Int) = {
    for(i <- 0 to (election(voter).preferences.length-1)) {
      if(election(voter).preferences(i) == candidate){
        return (true,i)
      }
    }
    return (false,0)
  }

  def utilityIndividual(election: Election[ACTBallot], voter: Int, candidate: Candidate): Int = rank(election,voter,candidate) match {
    case (true,rank) => return getCandidateList(election).length + 1 - rank
    case (false,_) => return 0
    return 0
  }

  def utilitySet(election: Election[ACTBallot], voter: Int, candidates: List[Candidate]): Int = {
    var sum: Int = 0
    for(i<- 0 to (candidates.length-1)){
      sum = sum + utilityIndividual(election, voter, candidates(i))
    }
    return sum
  }

  def socialWelfare(election: Election[ACTBallot], candidates: List[Candidate], fairness: Double): Double = {
    var sum: Double = 0
    for(i <- 0 to (election.length-1)){
      sum = sum + exp((1/fairness) * log((election(i).weight).toDouble * utilitySet(election,i,candidates)))
    }
    return sum
  }
}
