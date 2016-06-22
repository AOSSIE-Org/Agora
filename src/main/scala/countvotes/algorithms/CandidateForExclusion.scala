package countvotes.algorithms

import countvotes.structures._
import countvotes.methods._
import collection.mutable.{HashMap => Map}

trait ACTCandidateForExclusion extends ExclusionTieResolution {
  
  def chooseCandidateForExclusion(totals: Map[Candidate, Rational]): (Candidate, Rational) = {
    var min = new Rational(Int.MaxValue, 1)
    for (kv <- totals) if (kv._2 < min) min = kv._2
       
    resolveExclusionTie(totals filter {_._2 == min}) 
  }
  
 

}