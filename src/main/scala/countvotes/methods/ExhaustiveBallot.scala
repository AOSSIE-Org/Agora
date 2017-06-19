package countvotes.methods


import countvotes.structures._
import countvotes.algorithms._


import countvotes.methods.VoteCountingMethod

object ExhaustiveBallot extends VoteCountingMethod[WeightedBallot] {

  protected val result: Result = new Result
  protected val report: Report[WeightedBallot] = new Report[WeightedBallot]

  def exclude(election: Election[WeightedBallot],
              candidate: Candidate): (Election[WeightedBallot], Set[WeightedBallot] ) = {
    var list: Election[WeightedBallot]  = Nil
    var setExhausted: Set[WeightedBallot] = Set()
    for (b <- election if !b.preferences.isEmpty) {
      if (b.preferences.head == candidate ) {
        if (b.preferences.tail.nonEmpty) {
          list = WeightedBallot(b.preferences.tail,  b.id,  b.weight)::list
        }
        else {
          setExhausted += b
        }
      }
      else {
        list = WeightedBallot(b.preferences.head :: b.preferences.tail filter {
          _ != candidate
        }, b.id, b.weight) :: list
      }
    }

    (list, setExhausted)
  }

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int):   Report[WeightedBallot]  = {



    print("\n INPUT ELECTION: \n")
    printElection(election)

    val tls = totals(election, candidates) // Here are totals of candidates also not OCCURING in the ballots

    result.addTotalsToHistory(tls)

    //report.setCandidates(getCandidates(election))  // Here are candidates OCCURING in the election
    report.setCandidates(candidates)  // Here are candidates also not OCCURING in the election

    report.newCount(Input, None, Some(election), Some(tls), None, None)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  override def winners(election: Election[WeightedBallot], ccandidates: List[Candidate],  numVacancies: Int): List[(Candidate, Rational)] = {

    val tls = totals(election,ccandidates)
    if(tls.size>2){
      val losingCand =  tls.toList.sortWith(_._2>_._2).reverse.head
      //println(losingCand)
      val newElection = exclude(election,losingCand._1)._1
      winners(newElection, ccandidates.filterNot(x => x == losingCand._1), numVacancies)
    } else {
      tls.toList.sortWith(_._2>_._2).head::List()
    }
  }
}
