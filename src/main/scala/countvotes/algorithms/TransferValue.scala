package countvotes.algorithms

import countvotes.structures._
import countvotes.structures.{PreferenceBallot => Ballot}
import countvotes.methods._
import collection.mutable.{HashMap => Map}
import java.io._

import spire.math.Rational

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




trait TransferValueWithDenominatorWithNumOfMarkedContinuingBallots extends STV[ACTBallot]{
  def computeTransferValue(surplus: Rational,
                           election: Election[ACTBallot],
                           pendingWinners:  List[Candidate],
                           candidate: Candidate, markings: Option[Set[Int]]): Rational = {
    //println("Pending winners: " + pendingWinners)
    var num = 0
    markings match {
     case None => throw new Exception("Last parcel is undetermined.")
     case Some(mrks) =>
      for (b <- election if !b.preferences.isEmpty) {

        if ( b.preferences.head == candidate  && !b.preferences.tail.diff(pendingWinners).isEmpty  && mrks.contains(b.id)) {num = num + 1}
      }
      println("Surplus: " + surplus)
      println("Denominator: " + num)
      surplus/num
    }
  }
}


trait TransferValueWithDenominatorWithNumOfMarkedContinuingBallotsOrOne {
  def computeTransferValue(surplus: Rational,
                           election: Election[ACTBallot],
                           pendingWinners:  List[Candidate],
                           candidate: Candidate, markings: Option[Set[Int]]): Rational = {
    //println("TV with denominator with the cardinality of marked non-exhausted ballots")
    var num = 0
     markings match {
     case None => throw new Exception("Last parcel is undetermined.")
     case Some(mrks) =>
      for (b <- election if !b.preferences.isEmpty) {
        if ( b.preferences.head == candidate  && !b.preferences.tail.diff(pendingWinners).isEmpty  && mrks.contains(b.id)) {num = num + 1}
      }
    }
    if (num == 0) {
      println("Denominator is equal to 0 !!!!!!!!!!!!!!!!!!!!")
    }

    var tv: Rational = 1
    if (num != 0) tv = surplus/num
    if (tv > 1) 1 else tv
  }
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trait TransferValueWithDenominatorWithNumOfBallots extends STV[ACTBallot]{
  def computeTransferValue(surplus: Rational,
                           election: Election[ACTBallot],
                           pendingWinners:  List[Candidate],
                           candidate: Candidate, markings: Option[Set[Int]]): Rational = {
    var num = 0
      for (b <- election if !b.preferences.isEmpty) {
        if ( b.preferences.head == candidate ) {num = num + 1}
      }
      println("Denominator: " + num)
      println("Surplus: " + surplus)
      surplus/num
  }
}



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trait TransferValueWithDenominatorWithNumOfAllContinuingBallots extends STV[ACTBallot]{
  def computeTransferValue(surplus: Rational,
                           election: Election[ACTBallot],
                           pendingWinners:  List[Candidate],
                           candidate: Candidate, markings: Option[Set[Int]]): Rational = {
    var num = 0
      for (b <- election if !b.preferences.isEmpty) {
        if ( b.preferences.head == candidate  && !b.preferences.tail.diff(pendingWinners).isEmpty) {num = num + 1}
      }
      // println("Denominator: " + num)
      surplus/num
  }
}


trait TransferValueWithDenominatorWithNumOfAllContinuingBallotsOrOne extends STV[ACTBallot]{
  def computeTransferValue(surplus: Rational,
                           election: Election[ACTBallot],
                           pendingWinners:  List[Candidate],
                           candidate: Candidate, markings: Option[Set[Int]]): Rational = {
    var num = 0
      for (b <- election if !b.preferences.isEmpty) {
        if ( b.preferences.head == candidate  && !b.preferences.tail.diff(pendingWinners).isEmpty) {num = num + 1}
      }
    if (num == 0) {
      println("Denominator is equal to 0 !!!!!!!!!!!!!!!!!!!!")
    }

    var tv: Rational = 1
    if (num != 0) tv = surplus/num
    if (tv > 1) 1 else tv
  }
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// TV = surplus /  total of continuing ballot papers (i.e. with further continuing preferences)
trait TransferValueWithDenominatorWithTotalOfContinuingBallots extends STV[Ballot]{
  def computeTransferValue(surplus: Rational,
                           election: Election[Ballot],
                           pendingWinners:  List[Candidate],
                           candidate: Candidate, markings: Option[Set[Int]]): Rational = {
    var num:Rational = 0
      for (b <- election if !b.preferences.isEmpty) {
        if ( b.preferences.head == candidate  && !b.preferences.tail.diff(pendingWinners).isEmpty) {num = num + b.weight}
      }
      // println("Denominator: " + num)
      surplus/num
  }
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/*
 * TV =  surplus /  total
 * */
trait TransferValueWithDenominatorEqualToTotal extends STV[Ballot]{
  def computeTransferValue(surplus: Rational,
                           election: Election[Ballot],
                           pendingWinners:  List[Candidate],
                           candidate: Candidate, markings: Option[Set[Int]]): Rational = {
    surplus/computeTotal(election, candidate)
  }
}





