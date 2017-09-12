package agora.model

import scala.language.implicitConversions
import spire.math.Rational

abstract class Ballot(val id: Int, val weight: Rational)

case class PreferenceBallot(val preferences: List[Candidate], override val id: Int, override val weight: Rational)
extends Ballot(id, weight)

case class ScoreBallot(scores: List[(Candidate, Rational)], override val id: Int, w: Rational) 
extends Ballot(id, w) 

case class RankBallot(val ranks: List[(Candidate, Int)], override val id: Int, w: Rational) 
extends Ballot(id, w)

case class ApprovalBallot(val approvals: Set[Candidate], override val id: Int, w: Rational) 
extends Ballot(id, w)

// TODO: use ApprovalBallot in Approval voting