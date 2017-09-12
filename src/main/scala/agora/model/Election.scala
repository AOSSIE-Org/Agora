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
  
  def firstVotes(candidates: List[Candidate]): Map[Candidate, Rational] = {
    val m = new MMap[Candidate, Rational]

    for (b <- ballots; (c, t) <- b.firstVotes) m(c) = t * b.weight + m.getOrElse(c, 0)

    m
  }
  
  lazy val weight = ((ballots map {_.weight}) :\ Rational(0,1)) { _ + _ }
 
}
object Election {
  
  // utility method for matrix where a[i][j] = x means candidate i has got #x votes against candidate j
  def pairwiseComparison(election: Election[PreferenceBallot], candidates: List[Candidate]): Array[Array[Rational]] = {
    val responseMatrix = Array.fill(candidates.size, candidates.size)(Rational(0, 1))

    for (b <- election) {
      val pi = b.preferences.zipWithIndex
      for ( (c1,i1) <- pi; (c2,i2) <- pi.take(i1)) {
        responseMatrix(candidates.indexOf(c2))(candidates.indexOf(c1)) += b.weight
      }
    }      
     
    responseMatrix
  }
  
  // utility method for matrix where a[i][j] = x means candidate i has got #x votes against candidate j
  def pairwiseComparisonRank(election: Election[RankBallot], candidates: List[Candidate]): Array[Array[Rational]] = {
    val responseMatrix = Array.fill(candidates.size, candidates.size)(Rational(0, 1))
 
    for (b <- election) {
      val pi = b.sortedRanks.zipWithIndex
      for ( ((c1,s1),i1) <- pi; ((c2,s2),i2) <- pi.take(i1) if s1 != s2) {
        responseMatrix(candidates.indexOf(c2))(candidates.indexOf(c1)) += b.weight
      }
    }
    
    responseMatrix
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


