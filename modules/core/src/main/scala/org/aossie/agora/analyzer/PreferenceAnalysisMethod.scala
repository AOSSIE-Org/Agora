package org.aossie.agora.analyzer

import org.aossie.agora.model.{PreferenceBallot => Ballot, Candidate, Election}

/**
  * Created by deepeshpandey on 18/06/17.
  */
abstract class PreferenceAnalysisMethod[B <: Ballot] {

    def analyse(e: org.aossie.agora.model.Election[B], ccandidates: List[Candidate]): Boolean

}
