package org.aossie.agora.model

import spire.math.Rational

abstract class Ballot[+C <: Candidate](val id: Int, val weight: Rational) {

  def firstVotes[CC >: C]: Map[CC, Rational]

}

class PreferenceBallot[+C <: Candidate](
    val preferences: List[C],
    override val id: Int,
    override val weight: Rational
) extends Ballot[C](id, weight) {

  def firstVotes[CC >: C]: Map[CC, Rational] = preferences.headOption match {
    case Some(c) => Map(c -> Rational(1, 1))
    case None    => Map()
  }

}

case class ScoreBallot[+C <: Candidate](
    val scores: List[(C, Rational)],
    override val id: Int,
    w: Rational
) extends Ballot[C](id, w) {

  lazy val sortedScores = scores.sortWith((cs1, cs2) => cs1._2 > cs2._2)

  def firstVotes[CC >: C]: Map[CC, Rational] = sortedScores.headOption match {
    case Some((c, s)) => Map(c -> Rational(1, 1))
    case None         => Map()
  }

}

case class RankBallot[+C <: Candidate](val ranks: List[(C, Int)], override val id: Int, w: Rational)
    extends Ballot[C](id, w) {

  lazy val sortedRanks = ranks.sortWith((cs1, cs2) => cs1._2 < cs2._2)

  def firstVotes[CC >: C]: Map[CC, Rational] = sortedRanks.headOption match {
    case Some((c, s)) => Map(c -> Rational(1, 1))
    case None         => Map()
  }

}

case class ApprovalBallot[C <: Candidate](val approvals: Set[C], override val id: Int, w: Rational)
    extends Ballot[C](id, w) {

  def firstVotes[CC >: C]: Map[CC, Rational] = ???

}

// TODO: use ApprovalBallot in Approval voting
