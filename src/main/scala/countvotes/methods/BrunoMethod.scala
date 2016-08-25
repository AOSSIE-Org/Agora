package countvotes.methods

import countvotes.structures._
import countvotes.algorithms._
import scala.math._

object BrunoMethod extends BrunoMethodClass[ACTBallot] {
  var memo = Map[(Int,Set[Candidate]), List[Candidate]] ()

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

    // new method Winners to be used instead

    //for(i <- candidateSubsets){
    //  contemplatedMax = socialWelfare(election, i, fairness)
    //  if(currentMax < contemplatedMax){
    //    currentMax = contemplatedMax
    //    currentCandidates = i
    //  }
    //}

    currentCandidates = winners(candidateList, numVacancies, election, fairness)

    //To fit with the current return system
    var currentCandidatesForReturn : List[(Candidate,Rational)] = List.empty
    for(i <- currentCandidates){
      currentCandidatesForReturn = currentCandidatesForReturn :+ (i,new Rational(n=1,d=1))
    }
    currentCandidatesForReturn
  }

  def winners(candidateList: List[Candidate], numVacancies: Int, election: Election[ACTBallot], fairness: Double): List[Candidate] = numVacancies match {
    case 0 => List.empty
    case n => {
      if(memo.contains((numVacancies,candidateList.toSet))){
        println("Testing: recieved from memo")
        return memo((numVacancies,candidateList.toSet))
      }
      println("Testing: numVacancies " + numVacancies)
      println("Testing: candidateList " + candidateList)
      var contemplatedSets : List[List[Candidate]] = List.empty
      for(i <- candidateList){
        println("Testing: Inside generating step up sets loop")
        contemplatedSets = contemplatedSets :+ (winners(candidateList.filterNot(elem => elem == i), numVacancies-1, election, fairness) :+ i)
      }
      val contemplatedSetsWelfareTuple: List[(Double,List[Candidate])] = contemplatedSets.map(x => (socialWelfare(election, x, fairness),x))
      val result: List[Candidate] = (contemplatedSetsWelfareTuple.foldLeft ((0.0, candidateList)) ((x,y) => maxTuple1(x,y)))._2   //Error if all less than 0
      memo += (((numVacancies,candidateList.toSet),result))
      return result
    }
  }

  //Redundent with current winner implementation
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

  def getCandidateList(election: Election[ACTBallot]): List[Candidate] = {
    var candidateList:List[Candidate] = List()
    for(i <- election){
      for(j <- i.preferences){
        if(!(candidateList contains j)){
          candidateList = candidateList :+ j
        }
      }
    }
    candidateList
  }

  def rank(election: Election[ACTBallot], voter: Int, candidate: Candidate): (Boolean,Int) = {
    for(i <- 0 to (election(voter).preferences.length-1)) { //Kept in "old" format as the rank itself is returned. Seems to be the simplest way, instead of getting the candidates, then searching back for their index.
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
    val sum: Int = candidates map { c => utilityIndividual(election, voter, c)} reduce { _ + _ }
    sum
  }

  def socialWelfare(election: Election[ACTBallot], candidates: List[Candidate], fairness: Double): Double = {
    var sum: Double = 0
    for(i <- 0 to (election.length-1)){
      sum = sum + exp((1/fairness) * log((election(i).weight).toDouble * utilitySet(election,i,candidates)))
    }
    sum
  }

  def maxTuple1(x:(Double, List[Candidate]), y:(Double, List[Candidate])): (Double, List[Candidate]) = {
    if (x._1 > y._1) {x}
    else {y}
  }
