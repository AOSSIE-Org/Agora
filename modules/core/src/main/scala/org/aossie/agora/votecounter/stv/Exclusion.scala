package org.aossie.agora.votecounter.stv

import org.aossie.agora.model._
import org.aossie.agora.votecounter._
import spire.math.Rational

trait ACTExclusion[C <: Candidate] extends STV[C, ACTBallot] {

  def excludeZero(
      election: Election[C, ACTBallot],
      candidate: C
  ): (Election[C, ACTBallot], Set[ACTBallot[C]]) = {
    var list: List[ACTBallot[C]]        = Nil
    var setExhausted: Set[ACTBallot[C]] = Set()
    for (b <- election if b.preferences.nonEmpty)
      list = new ACTBallot[C](
        (b.preferences.head :: b.preferences.tail).filter(_ != candidate),
        b.id,
        false,
        b.weight,
        b.value
      ) :: list
    (Election(list), setExhausted)
  }

  def exclude(
      election: Election[C, ACTBallot],
      candidate: C,
      value: Option[Rational],
      newWinners: Option[List[C]]
  ): (Election[C, ACTBallot], Set[ACTBallot[C]]) = {
    var list: List[ACTBallot[C]]        = Nil
    var setExhausted: Set[ACTBallot[C]] = Set()
    value match {
      case None => throw new Exception("Argument value are missing in trait ACTExclusion")
      case Some(v) =>
        newWinners match {
          case None => throw new Exception("Argument newWinners are missing in trait ACTExclusion")
          case Some(nW) =>
            for (b <- election if b.preferences.nonEmpty) {
              if (b.preferences.head == candidate && b.value == v) {
                if (b.preferences.tail.nonEmpty) {
                  val restOfPreferences = filterPreferences(b.preferences.tail, candidate :: nW)
                  if (restOfPreferences.nonEmpty) {
                    list = new ACTBallot[C](restOfPreferences, b.id, true, b.value, b.value) :: list
                  } else {
                    setExhausted += b
                  }
                }
              } else {
                list = new ACTBallot[C](
                  b.preferences.head :: filterPreferences(
                    b.preferences.tail.filter {
                      _ != candidate
                    },
                    nW
                  ),
                  b.id,
                  false,
                  b.weight,
                  b.value
                ) :: list
              }
            }
        }
    }
    (Election(list), setExhausted)
  }

}

// exactly like ACTExclusion
trait SenateExclusion[C <: Candidate] extends STV[C, ACTBallot] {

  def excludeZero(
      election: Election[C, ACTBallot],
      candidate: C
  ): (Election[C, ACTBallot], Set[ACTBallot[C]]) = {
    var list: List[ACTBallot[C]]        = Nil
    var setExhausted: Set[ACTBallot[C]] = Set()
    for (b <- election if b.preferences.nonEmpty)
      list = new ACTBallot[C](
        (b.preferences.head :: b.preferences.tail).filter(_ != candidate),
        b.id,
        false,
        b.weight,
        b.value
      ) :: list
    (Election(list), setExhausted)
  }

  def exclude(
      election: Election[C, ACTBallot],
      candidate: C,
      value: Option[Rational],
      newWinners: Option[List[C]]
  ): (Election[C, ACTBallot], Set[ACTBallot[C]]) = {
    var list: List[ACTBallot[C]]        = Nil
    var setExhausted: Set[ACTBallot[C]] = Set()
    value match {
      case None => throw new Exception("Argument value are missing in trait ACTExclusion")
      case Some(v) =>
        newWinners match {
          case None => throw new Exception("Argument newWinners are missing in trait ACTExclusion")
          case Some(nW) =>
            for (b <- election if b.preferences.nonEmpty) {
              if (b.preferences.head == candidate && b.value == v) {
                if (b.preferences.tail.nonEmpty) {
                  val restOfPreferences = filterPreferences(b.preferences.tail, candidate :: nW)
                  if (restOfPreferences.nonEmpty) {
                    list = new ACTBallot[C](restOfPreferences, b.id, true, b.value, b.value) :: list
                  } else {
                    setExhausted += b
                  }
                }
              } else {
                list = new ACTBallot[C](
                  b.preferences.head :: filterPreferences(
                    b.preferences.tail.filter {
                      _ != candidate
                    },
                    nW
                  ),
                  b.id,
                  false,
                  b.weight,
                  b.value
                ) :: list
              }
            }
        }
    }
    (Election(list), setExhausted)
  }

}

trait SimpleExclusion[C <: Candidate] extends STV[C, PreferenceBallot] {

  def exclude(
      election: Election[C, PreferenceBallot],
      candidate: C,
      value: Option[Rational],
      newWinners: Option[List[C]]
  ): (Election[C, PreferenceBallot], Set[PreferenceBallot[C]]) = {
    var list: List[PreferenceBallot[C]]        = Nil
    var setExhausted: Set[PreferenceBallot[C]] = Set()
    for (b <- election if !b.preferences.isEmpty) {
      if (b.preferences.head == candidate) {
        if (b.preferences.tail.nonEmpty) {
          list = new PreferenceBallot[C](b.preferences.tail, b.id, b.weight) :: list
        } else {
          setExhausted += b
        }
      } else {
        list = new PreferenceBallot[C](
          (b.preferences.head :: b.preferences.tail).filter {
            _ != candidate
          },
          b.id,
          b.weight
        ) :: list
      }
    }

    (Election(list), setExhausted)
  }

}

trait SimpleExclusionWithFixedElectionSize[C <: Candidate] {

  // Removes the candidate from the ballot but does not reduce the election size by removing empty ballots
  def exclude(
      election: Election[C, PreferenceBallot],
      candidate: C
  ): Election[C, PreferenceBallot] = {
    Election {
      election.map { b =>
        val newPrefs = b.preferences.filter(_ != candidate)
        new PreferenceBallot(newPrefs, b.id, b.weight)
      }
    }
  }

}
