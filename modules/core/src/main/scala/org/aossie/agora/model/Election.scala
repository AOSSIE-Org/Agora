package org.aossie.agora.model

import scala.collection._
import scala.collection.mutable.{HashMap => MMap}
import spire.math.Rational

import scala.language.higherKinds
import scala.collection.compat.IterableOnce

class Election[C <: Candidate, B[CC >: C <: Candidate] <: Ballot[CC]](val ballots: Seq[B[C]])
    extends Seq[B[C]]
    with SeqOps[B[C], Seq, Election[C, B]] {

  // Mandatory overrides of `fromSpecific`, `newSpecificBuilder`,
  // and `empty`, from `IterableOps`

  override protected def fromSpecific(coll: IterableOnce[B[C]]): Election[C, B] =
    Election.fromSpecific(coll)

  override protected def newSpecificBuilder: mutable.Builder[B[C], Election[C, B]] =
    Election.newBuilder
  override def empty: Election[C, B] = Election(Nil)

  def iterator = ballots.iterator

  def apply(i: Int) = ballots(i)

  def length = ballots.length

  override def toString = ballots.map(_.toString).mkString("\n")

  def firstVotes(candidates: List[C]): Map[C, Rational] = {
    val m = new MMap[C, Rational]

    for {
      b      <- ballots
      (c, t) <- b.firstVotes
    } m(c) = t * b.weight + m.getOrElse(c, 0)

    m
  }

  lazy val weight = ballots.map(_.weight).foldRight(Rational(0, 1))(_ + _)

}

object Election {
  // Mandatory factory methods: `empty`, `newBuilder`
  // and `fromSpecific`

  def empty[C <: Candidate, B[CC >: C <: Candidate] <: Ballot[CC]]: Election[C, B] = apply(Nil)

  def newBuilder[C <: Candidate, B[CC >: C <: Candidate] <: Ballot[CC]]
      : mutable.Builder[B[C], Election[C, B]] =
    mutable.Seq.newBuilder[B[C]].mapResult(apply)

  def fromSpecific[C <: Candidate, B[CC >: C <: Candidate] <: Ballot[CC]](
      it: IterableOnce[B[C]]
  ): Election[C, B] = it match {
    case seq: collection.Seq[B[C]] => apply(seq)
    case _                         => apply(mutable.Seq.from(it))
  }

  // def apply[B <: Ballot](ballots: B*) = new Election(ballots)
  def apply[C <: Candidate, B[CC >: C <: Candidate] <: Ballot[CC]](
      ballots: Seq[B[C]]
  ): Election[C, B] =
    new Election(ballots)

}
