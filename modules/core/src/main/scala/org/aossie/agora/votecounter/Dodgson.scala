package org.aossie.agora.votecounter

import org.aossie.agora.model._
import spire.math.Rational

import collection.mutable.{HashMap => MMap}
import scala.collection.SeqView

/** Wiki : https://en.wikipedia.org/wiki/Dodgson%27s_method Implementation :
  * http://infosyncratic.nl/talks/2008-votingprocedures.pdf
  */
object Dodgson extends VoteCounter[PreferenceBallot] {

  override def winners[C <: Candidate](
      e: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {

    // find flip vector from min sum to max sum which satisfies condorcet condition
    val minDodgsonFlipList: SeqView[List[Int]] =
      List
        .fill(e.weight.toInt)(0 to ccandidates.length)
        .flatten
        .view
        .combinations(e.weight.toInt)
        .flatMap(_.toList.permutations)
        .toList
        .sortBy(_.sum)
        .view

    val minDodgsonFlip =
      minDodgsonFlipList.find(list => getCondorcetWinnerIfExist(list, ccandidates, e).nonEmpty)

    // find the dodgson winner based on this flip vector
    minDodgsonFlip match {
      case Some(list) =>
        getCondorcetWinnerIfExist(list, ccandidates, e) match {
          case Some(candidate) => List((candidate, Rational(list.sum, 1)))
          case None            => List()
        }
      case None => List()
    }
  }

  def getCondorcetWinnerIfExist[C <: Candidate](
      list: List[Int],
      candidates: List[C],
      election: Election[C, PreferenceBallot]
  ): Option[C] = {

    val dodgsonWinner = candidates.find(c =>
      getCandidateMajorityArray(election, c, list, candidates) match {
        case Some(matrix) => isCondorcetWinner(c, candidates, matrix, election.weight.toInt)
        case None         => false
      }
    )

    dodgsonWinner
  }

  // returns an array where the value at index i represents total votes to param candidate against candidates(i)
  // this is all required to calculate if the param candidate is condorcet winner or not
  def getCandidateMajorityArray[C <: Candidate](
      election: Election[C, PreferenceBallot],
      candidate: C,
      flipVector: List[Int],
      candidates: List[C]
  ): Option[Array[Int]] = {

    val dispersedElection = dispersed(election)

    assert(dispersedElection.length == flipVector.length)

    val candElectionResponse = Array.ofDim[Int](candidates.size)

    val succesful = dispersedElection.zip(flipVector).forall { ballotsWithFlip =>
      {

        if (isFlippable(candidate, ballotsWithFlip._2, ballotsWithFlip._1.preferences)) {

          // no need to generate new list with updated positions just compare using indices
          val prefs = ballotsWithFlip._1.preferences
          for (cand <- prefs) {
            if (
              (prefs.indexOf(cand) >= prefs.indexOf(
                candidate
              ) - ballotsWithFlip._2) && candidate != cand
            ) {
              candElectionResponse(candidates.indexOf(cand)) += 1
            }
          }
          true
        } else {
          false
        }
      }
    }

    if (succesful) {
      Option(candElectionResponse)
    } else {
      None
    }
  }

  private def cache[C <: Candidate] =
    new MMap[Election[C, PreferenceBallot], Election[C, PreferenceBallot]]()

  def dispersed[C <: Candidate](
      election: Election[C, PreferenceBallot]
  ): Election[C, PreferenceBallot] = {

    def disperseUtil[C <: Candidate](election: Election[C, PreferenceBallot]) = {
      Election {
        for {
          b <- election
          i <- 1 to b.weight.toInt
        } yield b
      }
    }
    cache.getOrElseUpdate(Election(election), disperseUtil(Election(election)))
  }

  def isCondorcetWinner(
      candidate: Candidate,
      candidates: List[Candidate],
      matrix: Array[Int],
      totalVoters: Int
  ): Boolean =
    matrix.zip(candidates).forall { case (score, cand) =>
      score > Rational(1, 2) * totalVoters || (cand == candidate)
    }

  def isFlippable(candidate: Candidate, rank: Int, ballot: List[Candidate]): Boolean =
    if (ballot.indexOf(candidate) - rank >= 0) true else false

}
