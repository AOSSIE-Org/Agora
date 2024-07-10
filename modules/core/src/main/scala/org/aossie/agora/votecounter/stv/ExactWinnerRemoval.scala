package org.aossie.agora.votecounter.stv

import org.aossie.agora.model._
import org.aossie.agora.model.{PreferenceBallot => Ballot}
import org.aossie.agora.votecounter._

trait ACTExactWinnerRemoval extends STV[Candidate, ACTBallot] {

  def removeWinnerWithoutSurplusFromElection(
      election: Election[Candidate, ACTBallot],
      winner: Candidate
  ): Election[Candidate, ACTBallot] = {
    var list: List[ACTBallot[Candidate]] = Nil
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
trait SenateExactWinnerRemoval extends STV[Candidate, ACTBallot] {

  def removeWinnerWithoutSurplusFromElection(
      election: Election[Candidate, ACTBallot],
      winner: Candidate
  ): Election[Candidate, ACTBallot] = {
    var list: List[ACTBallot[Candidate]] = Nil
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

trait ExactWinnerRemoval extends STV[Candidate, Ballot] {

  def removeWinnerWithoutSurplusFromElection(
      election: Election[Candidate, Ballot],
      winner: Candidate
  ): Election[Candidate, Ballot] = {
    var list: List[Ballot[Candidate]] = Nil
    for (b <- election if !b.preferences.isEmpty)
      if (b.preferences.head.name != winner.name) {
        list = Ballot(filterPreferences(b.preferences, winner :: List()), b.id, b.weight) :: list
      }
    Election(list)
  }

}
