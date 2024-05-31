package org.aossie.agora.model

import scala.collection._
import scala.collection.generic._
import scala.collection.mutable.Builder
import scala.collection.mutable.{HashMap => MMap}

import spire.math.Rational

class Election[+B <: Ballot](val ballots: Seq[B]) extends Seq[B] with SeqLike[B, Election[B]] {

  override def companion = ballots.companion

  def iterator = ballots.iterator

  def apply(i: Int) = ballots(i)

  def length = ballots.length

  override protected[this] def newBuilder = Election.newBuilder

  override def toString = ballots.map(_.toString).mkString("\n")

  def firstVotes(candidates: List[Candidate]): Map[Candidate, Rational] = {
    val m = new MMap[Candidate, Rational]

    for {
      b      <- ballots
      (c, t) <- b.firstVotes
    } m(c) = t * b.weight + m.getOrElse(c, 0)

    m
  }

  lazy val weight = ((ballots.map(_.weight)) :\ Rational(0, 1))(_ + _)

}

object Election {

  def newBuilder[B <: Ballot] = new mutable.Builder[B, Election[B]] {
    private[this] val base = Seq().genericBuilder[B]
    override def +=(e: B)  = { base += e; this }
    override def clear()   = base.clear()
    override def result()  = new Election[B](base.result())
  }

  implicit def canBuildFrom[B <: Ballot] = new CanBuildFrom[Election[_], B, Election[B]] {
    def apply(from: Election[_]): Builder[B, Election[B]] = newBuilder
    def apply(): Builder[B, Election[B]]                  = newBuilder
  }

  // def apply[B <: Ballot](ballots: B*) = new Election(ballots)
  def apply[B <: Ballot](ballots: Seq[B]) = new Election(ballots)

}
