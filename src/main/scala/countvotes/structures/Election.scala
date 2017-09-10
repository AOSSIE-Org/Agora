package countvotes.structures

import scala.collection._
import scala.collection.generic._
import scala.collection.mutable.{Builder, ListBuffer}
import scala.collection.mutable.{HashMap => MMap, HashSet => MSet}

import scala.language.implicitConversions

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
  
  def mentionedCandidates[B <: Ballot](election: Election[B]): List[Candidate] = {
    val set = new MSet[Candidate]()
    for (b <- election) {
      for (c <- b.preferences)
        if (!set.exists(n => n == c) ) set += c
    }
    set.toList
  }
  

  lazy val totalWeightedVoters = (election: Election[Ballot]) => {
    election filter { _.preferences.nonEmpty} map {_.weight} reduce { _ + _ }
  }

  implicit def weightedElectionToACTElection(we: Election[Ballot]): Election[ACTBallot] = {
    new Election(for (b <- we) yield ACTBallot.fromBallot(b)) // b // ACTBallot.fromBallot(b)
  }

  // FIXME: remove this implicit. Rank ballots cannot always be converted to Preferential Ballots.
  implicit def rankedElectionToWeightedElection(re: Election[RankBallot]): Election[Ballot] = {
    new Election(for (rb <- re) yield new Ballot(rb.ranks.map(_._1), rb.id, rb.weight))
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


