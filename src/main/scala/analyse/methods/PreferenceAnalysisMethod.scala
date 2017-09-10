package analyse.methods

import agora.structures.{PreferenceBallot => Ballot, Candidate, Election}

/**
  * Created by deepeshpandey on 18/06/17.
  */
abstract class PreferenceAnalysisMethod[B <: Ballot] {

    def analyse(e: agora.structures.Election[B], ccandidates: List[Candidate]): Boolean

}
