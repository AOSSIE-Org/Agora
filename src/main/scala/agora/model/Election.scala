package agora.model

import scala.collection._
import scala.collection.generic._
import scala.collection.mutable.{Builder, ListBuffer}
import scala.collection.mutable.{HashMap => MMap, HashSet => MSet}

import scala.language.implicitConversions
import spire.math.Rational
import agora.votecounter.stv.ACTBallot


class Election[+B <: Ballot](val ballots: Seq[B]) 
extends Seq[B]
with SeqLike[B, Election[B]] {
  override def companion = ballots.companion

  def iterator = ballots.iterator
  def apply(i: Int) = ballots(i)
  def length = ballots.length
  
  protected[this] override def newBuilder = Election.newBuilder 
  
  override def toString = ballots map { _.toString } mkString("\n")
  
}
object Election {
  
  def firstVotes[B <: Ballot](election: Election[B], candidates: List[Candidate]): MMap[Candidate, Rational] = {
    val m = new MMap[Candidate, Rational]

    for (c<-candidates) m(c) = 0

    for (b <- election; (c, t) <- b.firstVotes) {
      m(c) = t * b.weight + m(c)
    }
    m
  }
  
  // TODO: Use Breeze
  // utility method for matrix where a[i][j] = x means candidate i has got #x votes against candidate j
  def pairwiseComparison(election: Election[PreferenceBallot], candidates: List[Candidate]): Array[Array[Rational]] = {

    val zeroRational = Rational(0, 1)
    val responseMatrix = Array.fill(candidates.size, candidates.size)(Rational(0, 1))

    for (b <- election if b.preferences.nonEmpty) {
      b.preferences.zipWithIndex foreach { case (c1,i1) => {
        b.preferences.zipWithIndex foreach { case (c2,i2) => {
          if (i1 < i2) {
            responseMatrix(candidates.indexOf(c1))(candidates.indexOf(c2)) += b.weight
          }}}}}}
    responseMatrix
  }
  
  
  def totalWeightedVoters(election: Election[PreferenceBallot]) = {
    election filter { _.preferences.nonEmpty} map {_.weight} reduce { _ + _ }
  }

  implicit def weightedElectionToACTElection(we: Election[PreferenceBallot]): Election[ACTBallot] = {
    new Election(for (b <- we) yield ACTBallot.fromBallot(b)) // b // ACTBallot.fromBallot(b)
  }

  // FIXME: remove this implicit. Rank ballots cannot always be converted to Preferential Ballots.
  implicit def rankedElectionToWeightedElection(re: Election[RankBallot]): Election[PreferenceBallot] = {
    new Election(for (rb <- re) yield new PreferenceBallot(rb.ranks.map(_._1), rb.id, rb.weight))
  }
  
  
  def newBuilder[B <: Ballot] = new mutable.Builder[B, Election[B]] {
    private[this] val base = Seq().genericBuilder[B]
    override def +=(e: B) = { base += e; this }
    override def clear() = base.clear()
    override def result() = new Election[B](base.result())
  }
  
  implicit def canBuildFrom[B <: Ballot] = new CanBuildFrom[Election[_], B, Election[B]] {
    def apply(from: Election[_]): Builder[B, Election[B]] = newBuilder
    def apply(): Builder[B, Election[B]] = newBuilder
  }
  
  //def apply[B <: Ballot](ballots: B*) = new Election(ballots)
  def apply[B <: Ballot](ballots: Seq[B]) = new Election(ballots)
}


