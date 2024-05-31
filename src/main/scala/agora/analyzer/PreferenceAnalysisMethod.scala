package agora.analyzer

import agora.model.{PreferenceBallot => Ballot}
import agora.model.Candidate
import agora.model.Election

/** Created by deepeshpandey on 18/06/17. */
abstract class PreferenceAnalysisMethod[B <: Ballot] {

  def analyse(e: agora.model.Election[B], ccandidates: List[Candidate]): Boolean

}
