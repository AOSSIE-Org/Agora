package org.aossie.agora.comparator

import org.aossie.agora.model.{PreferenceBallot => Ballot, _}
import spire.math.Rational

import scala.language.higherKinds

/** A proper definition of strategyproofness for irresolute social choice functions requires the
  * specification of preferences over sets of alternatives. One way to obtain such preferences is to
  * extend the preferences that voters have over individual alternatives to (not necessarily
  * complete) preference relations over sets. A function that yields a preference relation over
  * subsets of alternatives when given a preference relation over single alternatives is called a
  * set extension. This implementation includes two of the natural and well studied set extension
  * methods - Kellys and Fishburn's extension methods.
  */
abstract class SetExtensionMethods[C <: Candidate, B[CC >: C <: Candidate] <: Ballot[CC]] {

  // will return the set that is preferred over another as given in the json parameters file
  def compare(
      election: org.aossie.agora.model.Election[C, B],
      candidates: List[C],
      parameters: Parameters
  ): Set[C]

  // utility method for matrix where a[i][j] = x means candidate i has got #x votes against candidate j
  def getPairwiseComparisons(
      election: Election[C, B],
      candidates: List[C]
  ): Array[Array[Rational]] = {

    val zeroRational   = Rational(0, 1)
    val responseMatrix = Array.fill(candidates.size, candidates.size)(Rational(0, 1))

    for (b <- election if b.preferences.nonEmpty) {
      b.preferences.zipWithIndex.foreach { case (c1, i1) =>
        b.preferences.zipWithIndex.foreach { case (c2, i2) =>
          if (i1 < i2) {
            responseMatrix(candidates.indexOf(c1))(candidates.indexOf(c2)) += b.weight
          }
        }
      }
    }
    responseMatrix
  }

}
