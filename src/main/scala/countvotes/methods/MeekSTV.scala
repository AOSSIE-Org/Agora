package countvotes.methods

import countvotes.structures._
import countvotes.algorithms._
import collection.mutable.{HashMap => MMap}

object MeekSTV extends STV[WeightedBallot]
  with DroopQuota // Imp
  with NoFractionInQuota // Imp
  with NewWinnersNotOrdered[WeightedBallot]
  with SimpleSurplusDistributionTieResolution // not necessary because of NewWinnersNotOrdered
  with SimpleExclusion
  with UnfairExclusionTieResolutuim
  with TransferValueWithDenominatorEqualToTotal
  with ScrutinyWithAllBallotsInSurplusDistribution
  with ExactWinnerRemoval {


  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int): Report[WeightedBallot] = {
    val quota = cutQuotaFraction(computeQuota(election.length, numVacancies))
    println("Quota = " + quota)
    result.setQuota(quota)

    var keepFactor = new MMap[Candidate, Rational]
    for (c <- candidates) {
      keepFactor(c) = Rational(1, 1)
    }

    result.setKF(keepFactor)


    print("\n INPUT ELECTION: \n")
    printElection(election)

    val tls = totals(election, candidates) // Here are totals of candidates also not OCCURING in the ballots
    result.addTotalsToHistory(tls)

    //report.setCandidates(getCandidates(election))  // Here are candidates OCCURING in the election
    report.setCandidates(candidates) // Here are candidates also not OCCURING in the election

    report.newCount(Input, None, Some(election), Some(tls), None, None)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }

  def totalsMeek(election: Election[WeightedBallot], ccandidates: List[Candidate], flags: MMap[Candidate, Rational]): MMap[Candidate, Rational] ={
    var m = new MMap[Candidate, Rational]
    for(c<-ccandidates){
      m(c) = Rational(0,1)
    }
    for(b<-election if !b.preferences.isEmpty){
      var multiplier = Rational(1,1)
      for(c<-b.preferences){
        m(c) = m.getOrElse(c, Rational(0,1)) + b.weight*multiplier*flags(c)
        multiplier = multiplier * (Rational(1,1)- flags(c))
      }
    }
    m
  }

  def surplusCandidates(totals: MMap[Candidate, Rational], quota: Rational): Int = {
    var surplusCandidatesNumber = totals.filter(x => x._2 >= quota).size
    surplusCandidatesNumber
  }

  def surplusQuantity(totals: MMap[Candidate, Rational], quota: Rational): Rational = {
    var surplusAmount: Rational = Rational(0,1)
    totals.filter(x => x._2 >= quota).foreach(x => {
      surplusAmount = surplusAmount + x._2 - quota} )
    surplusAmount
  }

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  override def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    println(" \n NEW RECURSIVE CALL \n")

    var ccands = ccandidates
    var flag = result.getKF
    val tls = totalsMeek(election, ccandidates, flag)

    if (ccands.length <= numVacancies) {
      for (c <- ccands) yield (c, tls(c))
    }
    else {
      if (surplusCandidates(tls, result.getQuota) >= numVacancies) {
        tls.toList.sortWith(_._2>_._2).take(numVacancies)
      } else {
        //Find surplus and check if surplus + last candidate's number of votes < quota
        //If so, then KV = 0 for them
        //Else find surplus ones and reduce their KV
        var surplusAmount = surplusQuantity(tls, result.getQuota)
        if (tls.toList.filter(x => ccandidates.contains(x._1)).sortWith(_._2 < _._2).head._2 + surplusAmount < tls.toList.filter(x => ccandidates.contains(x._1)).sortWith(_._2 < _._2).tail.head._2) {
          flag(tls.toList.filter(x => ccandidates.contains(x._1)).sortWith(_._2 < _._2).head._1) = Rational(0, 1)
          winners(election, ccandidates.filterNot(_ == tls.toList.filter(x => ccandidates.contains(x._1)).sortWith(_._2 < _._2).head._1), numVacancies)
        } else {
          var winnerList = tls.filter(x => ccandidates.contains(x._1)).filter(_._2 > result.getQuota)
          for (w <- winnerList) {
            flag(w._1) = flag(w._1) * Rational(w._2.denominator.toInt * result.getQuota.numerator.toInt, w._2.numerator.toInt)
          }
          winners(election, ccandidates, numVacancies)
        }
      }
    }
  }
}
