package agora.votecounter

import agora.model._
import agora.model.{PreferenceBallot => Ballot}
import spire.math.Rational

import scala.collection.mutable.{HashMap => MMap}

object EgalitarianDP extends Egalitarian[Ballot] {
  val memo = new MMap[(Int,Set[Candidate]), List[Candidate]] ()

  def winners(election: Election[Ballot],  ccandidates: List[Candidate], numVacancies: Int): List[(Candidate,Rational)] = {
    val candidateCount: Int = ccandidates.length
    if(candidateCount < numVacancies) {println("not enough candidates") }

    var winningCandidates: List[Candidate] = List.empty
    winningCandidates = recursiveWinnersComputation(ccandidates, numVacancies, election, candidateCount)

    var candidatesForReturn : List[(Candidate,Rational)] = List.empty
    for(i <- winningCandidates){
      candidatesForReturn = candidatesForReturn :+ (i, Rational(n=1,d=1))
    }
    candidatesForReturn
  }

  def recursiveWinnersComputation(candidateList: List[Candidate], numVacancies: Int, election: Election[Ballot], numCandidates: Int): List[Candidate] = numVacancies match {
    case 0 => List.empty
    case n => {
      if(memo.contains((numVacancies,candidateList.toSet))){
        memo((numVacancies,candidateList.toSet))
      }
      var contemplatedSets : List[List[Candidate]] = List.empty
      for(i <- candidateList){
        contemplatedSets = contemplatedSets :+ (recursiveWinnersComputation(candidateList.filterNot(elem => elem == i), numVacancies-1, election, numCandidates) :+ i)
      }
      val contemplatedSetsWelfareTuple: List[(Double,List[Candidate])] = contemplatedSets.map(x => (socialWelfare(election, x, numCandidates),x))
      val result: List[Candidate] = (contemplatedSetsWelfareTuple.foldLeft ((0.0, candidateList)) ((x,y) => maxTuple1(x,y)))._2   //Error if all less than 0
      memo += (((numVacancies,candidateList.toSet),result))
      result
    }
  }
}
