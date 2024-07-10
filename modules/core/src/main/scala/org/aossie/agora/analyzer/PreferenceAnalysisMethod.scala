package org.aossie.agora.analyzer

import org.aossie.agora.model.{PreferenceBallot => Ballot}
import org.aossie.agora.model.Candidate
import org.aossie.agora.model.Election

/** Created by deepeshpandey on 18/06/17. */
abstract class PreferenceAnalysisMethod[C <: Candidate, B[CC >: C <: Candidate] <: Ballot[CC]] {

  def analyse(
      e: Election[C, B],
      ccandidates: List[C]
  ): Boolean

}
