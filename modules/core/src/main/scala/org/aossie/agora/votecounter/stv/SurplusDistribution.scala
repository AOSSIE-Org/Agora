package org.aossie.agora.votecounter.stv

import org.aossie.agora.model._
import org.aossie.agora.model.{PreferenceBallot => Ballot}
import org.aossie.agora.votecounter._

import spire.math.Rational

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// * ACT Electoral act 1992:
//
// 1A + 6:
// 1A(1): For this schedule, "count votes", in relation to a candidate, means the number of votes worked out as follows: BPxTV
// 1A(2): However, any fraction is to be disregarded.
// 1A(3): In this clause:
// "BP" means the number of ballot papers to be dealt with at a count that record the next available preference for the candidate.
// "TV" means the transfer value of those ballot papers.
// 6(2)     EACH ballot paper COUNTED for the purpose of allotting votes to the successful candidate at the count at which the candidate became successful shall be dealt with as follows:
// 6(2)(a)     if it does not specify a next available preference—it shall be set aside as finally dealt with for this part;
// 6(2)(b)     if it specifies a next available preference—it shall be grouped according to the candidate for whom that preference is recorded.
// 6(3): The count votes for each continuing candidate shall be determined and allotted to him or her.
// 6(4)  After the allotment under subclause 6(3), the continuing candidates' total votes shall be calculated and, if the total votes of a candidate equal or exceed the quota, the candidate is successful.

// * ACT Electoral act 1992: 1C(2) + 1C(4):
// 1C(4): However, if the transfer value of a ballot paper worked out in accordance with subclause (2) would be greater than the transfer value of the ballot paper when counted for the successful candidate,
// the transfer value of that ballot paper is the transfer value of the ballot paper when counted for the successful candidate.
// i.e.:
// if (tv > weight_i) then weight_(i+1) = weight_i else weight_(i+1) = tv
//
trait ACTSurplusDistribution[C <: Candidate] extends STV[C, ACTBallot] {

  def distributeSurplusVotes(
      election: Election[C, ACTBallot],
      candidate: C,
      total: Rational,
      markings: Option[Set[Int]],
      pendingWinners: List[C],
      transferValue: Rational
  ): (Election[C, ACTBallot], Set[ACTBallot[C]], Option[Election[C, ACTBallot]]) = {
    var list: List[ACTBallot[C]]        = Nil
    var listIgnored: List[ACTBallot[C]] = Nil
    var setExhausted: Set[ACTBallot[C]] = Set()
    markings match {
      case None => throw new Exception("Last parcel is undetermined.")
      case Some(mrks) =>
        for (b <- election if !b.preferences.isEmpty) {

          if (b.preferences.head == candidate) {

            val continuingPreferences =
              filterPreferences(b.preferences.tail, candidate :: pendingWinners)
            if (continuingPreferences.nonEmpty) {
              // NOTE: HERE WE IGNORE BALLOTS THAT HAVE candidate AS FP BUT ARE NOT MARKED. THESE BALLOTS BECOME OUT OF SCRUTINY:
              if (mrks.contains(b.id)) {
                if (transferValue > b.value) { // 1C(4) of the ACT Electoral act 1992 Schedule 4
                  list = ACTBallot(
                    continuingPreferences,
                    b.id,
                    true,
                    b.value,
                    b.value
                  ) :: list // take care of b.weight (4th argument) here
                } else {
                  list = ACTBallot(
                    continuingPreferences,
                    b.id,
                    true,
                    transferValue,
                    transferValue
                  ) :: list // take care of b.weight  (4th argument) here
                }
              } else {
                listIgnored = b :: listIgnored // this ballot is lost because it does not belong to the last parcel
              }
            } else {
              setExhausted += b // this ballot is exhausted
            }
          } else {
            list = ACTBallot(
              b.preferences.head :: filterPreferences(
                b.preferences.tail.filter(_ != candidate),
                pendingWinners
              ),
              b.id,
              false,
              b.weight,
              b.value
            ) :: list
          }
        }
    }
    // println("setExhausted " + setExhausted)
    // println("listIgnored " + listIgnored)
    (Election(list), setExhausted, Some(Election(listIgnored)))
  }

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Section 273 (9)
trait SenateSurplusDistribution[C <: Candidate] extends STV[C, ACTBallot] {

  def distributeSurplusVotes(
      election: Election[C, ACTBallot],
      candidate: C,
      total: Rational,
      markings: Option[Set[Int]],
      pendingWinners: List[C],
      transferValue: Rational
  ): (Election[C, ACTBallot], Set[ACTBallot[C]], Option[Election[C, ACTBallot]]) = {
    var list: List[ACTBallot[C]]        = Nil
    var listIgnored: List[ACTBallot[C]] = Nil
    var setExhausted: Set[ACTBallot[C]] = Set()

    for (b <- election if !b.preferences.isEmpty) {
      if (b.preferences.head == candidate) {
        val continuingPreferences =
          filterPreferences(b.preferences.tail, candidate :: pendingWinners)
        if (continuingPreferences.nonEmpty) {
          list = ACTBallot(continuingPreferences, b.id, true, transferValue, transferValue) :: list
        } else {
          setExhausted += b // this ballot is exhausted
        }
      } else {
        list = ACTBallot(
          b.preferences.head :: filterPreferences(
            b.preferences.tail.filter {
              _ != candidate
            },
            pendingWinners
          ),
          b.id,
          false,
          b.weight,
          b.value
        ) :: list
      }
    }
    // println("setExhausted " + setExhausted)
    // println("listIgnored " + listIgnored)
    (Election(list), setExhausted, Some(Election(listIgnored)))
  }

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trait ACTVoteCounterWithAllContinuingBallotsInSurplusDistribution[C <: Candidate]
    extends STV[C, ACTBallot] {

  def distributeSurplusVotes(
      election: Election[C, ACTBallot],
      candidate: C,
      total: Rational,
      markings: Option[Set[Int]],
      pendingWinners: List[C],
      transferValue: Rational
  ): (Election[C, ACTBallot], Set[ACTBallot[C]], Option[Election[C, ACTBallot]]) = {

    var list: List[ACTBallot[C]]        = Nil
    var setExhausted: Set[ACTBallot[C]] = Set()

    for (b <- election if !b.preferences.isEmpty) {

      if (b.preferences.head == candidate) {
        val continuingPreferences =
          filterPreferences(b.preferences.tail, candidate :: pendingWinners)
        if (continuingPreferences.nonEmpty) {
          if (transferValue > b.value) { // 1C(4) of the ACT Electoral act 1992 Schedule 4
            list = ACTBallot(
              continuingPreferences,
              b.id,
              true,
              b.value,
              b.value
            ) :: list // take care of b.weight (4th argument) here
          } else {
            list = ACTBallot(
              continuingPreferences,
              b.id,
              true,
              transferValue,
              transferValue
            ) :: list // take care of b.weight  (4th argument) here
          }
        } else {
          setExhausted += b // this ballot is exhausted
        }
      } else {
        list = ACTBallot(
          b.preferences.head :: filterPreferences(
            b.preferences.tail.filter {
              _ != candidate
            },
            pendingWinners
          ),
          b.id,
          false,
          b.weight,
          b.value
        ) :: list
      }
    }
    (Election(list), setExhausted, None)
  }

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trait VoteCounterWithAllContinuingBallotsInSurplusDistribution[C <: Candidate]
    extends STV[C, Ballot] {

  def distributeSurplusVotes(
      election: Election[C, Ballot],
      candidate: C,
      total: Rational,
      markings: Option[Set[Int]],
      pendingWinners: List[C],
      transferValue: Rational
  ): (Election[C, Ballot], Set[Ballot[C]], Option[Election[C, Ballot]]) = {

    var list: List[Ballot[C]]        = Nil
    var setExhausted: Set[Ballot[C]] = Set()

    for (b <- election if !b.preferences.isEmpty) {

      if (b.preferences.head == candidate) {
        val continuingPreferences =
          filterPreferences(b.preferences.tail, candidate :: pendingWinners)
        if (continuingPreferences.nonEmpty) {
          list = new Ballot(continuingPreferences, b.id, b.weight * transferValue) :: list
        } else {
          setExhausted += b // this ballot is exhausted
        }
      } else {
        list = new Ballot(
          b.preferences.head :: filterPreferences(
            b.preferences.tail.filter {
              _ != candidate
            },
            pendingWinners
          ),
          b.id,
          b.weight
        ) :: list
      }
    }
    (Election(list), setExhausted, None)
  }

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trait VoteCounterWithAllBallotsInSurplusDistribution[C <: Candidate] extends STV[C, Ballot] {

  def distributeSurplusVotes(
      election: Election[C, Ballot],
      candidate: C,
      total: Rational,
      markings: Option[Set[Int]],
      pendingWinners: List[C],
      transferValue: Rational
  ): (Election[C, Ballot], Set[Ballot[C]], Option[Election[C, Ballot]]) = {

    var list: List[Ballot[C]]        = Nil
    var setExhausted: Set[Ballot[C]] = Set()

    for (b <- election if !b.preferences.isEmpty) {

      if (b.preferences.head == candidate) {
        if (b.preferences.tail.nonEmpty) {
          list = new Ballot(b.preferences.tail, b.id, b.weight * transferValue) :: list
        } else {
          setExhausted += b // this ballot is exhausted
        }
      } else {
        list = new Ballot(
          (b.preferences.head :: b.preferences.tail).filter {
            _ != candidate
          },
          b.id,
          b.weight
        ) :: list
      }
    }
    (Election(list), setExhausted, None)
  }

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
