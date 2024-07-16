package org.aossie.agora.model

import scala.collection._
import scala.collection.generic._
import scala.collection.mutable.Builder
import scala.collection.mutable.{HashMap => MMap}
import spire.math.Rational

import scala.language.higherKinds

class Election[C <: Candidate, B[CC >: C <: Candidate] <: Ballot[CC]](val ballots: Seq[B[C]])
    extends Seq[B[C]]
    with SeqLike[B[C], Election[C, B]] {

  override def companion = ballots.companion

  def iterator = ballots.iterator

  def apply(i: Int) = ballots(i)

  def length = ballots.length

  override protected[this] def newBuilder = Election.newBuilder

  override def toString = ballots.map(_.toString).mkString("\n")

  def firstVotes(candidates: List[C]): Map[C, Rational] = {
    val m = new MMap[C, Rational]

    for {
      b      <- ballots
      (c, t) <- b.firstVotes
    } m(c) = t * b.weight + m.getOrElse(c, 0)

    m
  }

  lazy val weight = ((ballots.map(_.weight)) :\ Rational(0, 1))(_ + _)

}

object Election {

  def newBuilder[C <: Candidate, B[CC >: C <: Candidate] <: Ballot[CC]] =
    new mutable.Builder[B[C], Election[C, B]] {
      private[this] val base   = Seq().genericBuilder[B[C]]
      override def +=(e: B[C]) = { base += e; this }
      override def clear()     = base.clear()
      override def result()    = new Election[C, B](base.result())
    }

  implicit def canBuildFrom[C <: Candidate, B[CC >: C <: Candidate] <: Ballot[CC]]
      : CanBuildFrom[Election[C, B], B[C], Election[C, B]] =
    new CanBuildFrom[Election[C, B], B[C], Election[C, B]] {
      def apply(from: Election[C, B]): Builder[B[C], Election[C, B]] = newBuilder[C, B]
      def apply(): Builder[B[C], Election[C, B]]                     = newBuilder[C, B]
    }

  // def apply[B <: Ballot](ballots: B*) = new Election(ballots)
  def apply[C <: Candidate, B[CC >: C <: Candidate] <: Ballot[CC]](ballots: Seq[B[C]]) =
    new Election(ballots)

}
