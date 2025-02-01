package org.aossie.agora.votecounter.stv

import org.aossie.agora.model._
import org.aossie.agora.model.{PreferenceBallot => Ballot}
import org.aossie.agora.votecounter._

import spire.math.Rational

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trait TransferValueWithDenominatorWithNumOfMarkedContinuingBallots[C <: Candidate]
    extends STV[C, ACTBallot] {

  def computeTransferValue(
      surplus: Rational,
      election: Election[C, ACTBallot],
      pendingWinners: List[C],
      candidate: C,
      markings: Option[Set[Int]]
  ): Rational = {
    // println("Pending winners: " + pendingWinners)
    var num = 0
    markings match {
      case None => throw new Exception("Last parcel is undetermined.")
      case Some(mrks) =>
        for (b <- election if !b.preferences.isEmpty) {

          if (
            b.preferences.head == candidate && !b.preferences.tail
              .diff(pendingWinners)
              .isEmpty && mrks.contains(b.id)
          ) { num = num + 1 }
        }
        println("Surplus: " + surplus)
        println("Denominator: " + num)
        surplus / num
    }
  }

}

trait TransferValueWithDenominatorWithNumOfMarkedContinuingBallotsOrOne[C <: Candidate] {

  def computeTransferValue(
      surplus: Rational,
      election: Election[C, ACTBallot],
      pendingWinners: List[C],
      candidate: C,
      markings: Option[Set[Int]]
  ): Rational = {
    // println("TV with denominator with the cardinality of marked non-exhausted ballots")
    var num = 0
    markings match {
      case None => throw new Exception("Last parcel is undetermined.")
      case Some(mrks) =>
        for (b <- election if !b.preferences.isEmpty) {
          if (
            b.preferences.head == candidate && !b.preferences.tail
              .diff(pendingWinners)
              .isEmpty && mrks.contains(b.id)
          ) { num = num + 1 }
        }
    }
    if (num == 0) {
      println("Denominator is equal to 0 !!!!!!!!!!!!!!!!!!!!")
    }

    var tv: Rational = 1
    if (num != 0) tv = surplus / num
    if (tv > 1) 1 else tv
  }

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trait TransferValueWithDenominatorWithNumOfBallots[C <: Candidate] extends STV[C, ACTBallot] {

  def computeTransferValue(
      surplus: Rational,
      election: Election[C, ACTBallot],
      pendingWinners: List[C],
      candidate: C,
      markings: Option[Set[Int]]
  ): Rational = {
    var num = 0
    for (b <- election if !b.preferences.isEmpty)
      if (b.preferences.head == candidate) { num = num + 1 }
    println("Denominator: " + num)
    println("Surplus: " + surplus)
    surplus / num
  }

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trait TransferValueWithDenominatorWithNumOfAllContinuingBallots[C <: Candidate]
    extends STV[C, ACTBallot] {

  def computeTransferValue(
      surplus: Rational,
      election: Election[C, ACTBallot],
      pendingWinners: List[C],
      candidate: C,
      markings: Option[Set[Int]]
  ): Rational = {
    var num = 0
    for (b <- election if !b.preferences.isEmpty) {
      if (b.preferences.head == candidate && !b.preferences.tail.diff(pendingWinners).isEmpty) {
        num = num + 1
      }
    }
    // println("Denominator: " + num)
    surplus / num
  }

}

trait TransferValueWithDenominatorWithNumOfAllContinuingBallotsOrOne[C <: Candidate]
    extends STV[C, ACTBallot] {

  def computeTransferValue(
      surplus: Rational,
      election: Election[C, ACTBallot],
      pendingWinners: List[C],
      candidate: C,
      markings: Option[Set[Int]]
  ): Rational = {
    var num = 0
    for (b <- election if !b.preferences.isEmpty) {
      if (b.preferences.head == candidate && !b.preferences.tail.diff(pendingWinners).isEmpty) {
        num = num + 1
      }
    }
    if (num == 0) {
      println("Denominator is equal to 0 !!!!!!!!!!!!!!!!!!!!")
    }

    var tv: Rational = 1
    if (num != 0) tv = surplus / num
    if (tv > 1) 1 else tv
  }

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// TV = surplus /  total of continuing ballot papers (i.e. with further continuing preferences)
trait TransferValueWithDenominatorWithTotalOfContinuingBallots[C <: Candidate]
    extends STV[C, Ballot] {

  def computeTransferValue(
      surplus: Rational,
      election: Election[C, Ballot],
      pendingWinners: List[C],
      candidate: C,
      markings: Option[Set[Int]]
  ): Rational = {
    var num: Rational = 0
    for (b <- election if !b.preferences.isEmpty) {
      if (b.preferences.head == candidate && !b.preferences.tail.diff(pendingWinners).isEmpty) {
        num = num + b.weight
      }
    }
    // println("Denominator: " + num)
    surplus / num
  }

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/*
 * TV =  surplus /  total
 * */
trait TransferValueWithDenominatorEqualToTotal[C <: Candidate] extends STV[C, Ballot] {

  def computeTransferValue(
      surplus: Rational,
      election: Election[C, Ballot],
      pendingWinners: List[C],
      candidate: C,
      markings: Option[Set[Int]]
  ): Rational =
    surplus / computeTotal(election, candidate)

}
