package agora.model

import scala.language.implicitConversions
import spire.math.Rational

abstract class Ballot(val id: Int, val weight: Rational) {

  def firstVotes: Map[Candidate, Rational]

}

case class PreferenceBallot(
    val preferences: List[Candidate],
    override val id: Int,
    override val weight: Rational
) extends Ballot(id, weight) {

  lazy val firstVotes = preferences.headOption match {
    case Some(c) => Map(c -> Rational(1, 1))
    case None    => Map()
  }

}

case class ScoreBallot(scores: List[(Candidate, Rational)], override val id: Int, w: Rational)
    extends Ballot(id, w) {

  lazy val sortedScores = scores.sortWith((cs1, cs2) => cs1._2 > cs2._2)

  lazy val firstVotes = sortedScores.headOption match {
    case Some((c, s)) => Map(c -> Rational(1, 1))
    case None         => Map()
  }

}

case class RankBallot(val ranks: List[(Candidate, Int)], override val id: Int, w: Rational)
    extends Ballot(id, w) {

  lazy val sortedRanks = ranks.sortWith((cs1, cs2) => cs1._2 < cs2._2)

  lazy val firstVotes = sortedRanks.headOption match {
    case Some((c, s)) => Map(c -> Rational(1, 1))
    case None         => Map()
  }

}

case class ApprovalBallot(val approvals: Set[Candidate], override val id: Int, w: Rational)
    extends Ballot(id, w) {

  lazy val firstVotes = ???

}

// TODO: use ApprovalBallot in Approval voting
