package org.aossie.agora.votecounter.stv

import org.aossie.agora.model._
import org.aossie.agora.model.{PreferenceBallot => Ballot}
import org.aossie.agora.votecounter._

trait ACTExactWinnerRemoval[C <: Candidate] extends STV[C, ACTBallot] {

  def removeWinnerWithoutSurplusFromElection(
      election: Election[C, ACTBallot],
      winner: C
  ): Election[C, ACTBallot] = {
    var list: List[ACTBallot[C]] = Nil
    for (b <- election if !b.preferences.isEmpty)
      if (b.preferences.head.name != winner.name) {
        list = ACTBallot(
          filterPreferences(b.preferences, winner :: List()),
          b.id,
          b.marking,
          b.weight,
          b.value
        ) :: list
      }
    Election(list)
  }

}

// exactly like ACTExactWinnerRemoval
trait SenateExactWinnerRemoval[C <: Candidate] extends STV[C, ACTBallot] {

  def removeWinnerWithoutSurplusFromElection(
      election: Election[C, ACTBallot],
      winner: C
  ): Election[C, ACTBallot] = {
    var list: List[ACTBallot[C]] = Nil
    for (b <- election if !b.preferences.isEmpty)
      if (b.preferences.head.name != winner.name) {
        list = ACTBallot(
          filterPreferences(b.preferences, winner :: List()),
          b.id,
          b.marking,
          b.weight,
          b.value
        ) :: list
      }
    Election(list)
  }

}

trait ExactWinnerRemoval[C <: Candidate] extends STV[C, PreferenceBallot] {

  def removeWinnerWithoutSurplusFromElection(
      election: Election[C, Ballot],
      winner: C
  ): Election[C, Ballot] = {
    var list: List[Ballot[C]] = Nil
    for (b <- election if !b.preferences.isEmpty)
      if (b.preferences.head.name != winner.name) {
        list =
          new Ballot(filterPreferences(b.preferences, winner :: List()), b.id, b.weight) :: list
      }
    Election(list)
  }

}
