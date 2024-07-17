package org.aossie.agora.votecounter

import org.aossie.agora.model.Candidate
import org.aossie.agora.votecounter.stv._

class EVACSnoLP[C <: Candidate]
    extends ACT[C]
    with TransferValueWithDenominatorWithNumOfAllContinuingBallotsOrOne[C] // instead of TransferValueWithDenominatorWithNumOfMarkedContinuingBallotsOrOne
    with ACTVoteCounterWithAllContinuingBallotsInSurplusDistribution[C]    // instead  ACTSurplusDistribution
    with ACTNewWinnersDuringSurplusesDistribution[C]
    with ACTNewWinnersDuringExclusion[C]
    with ACTTotalsDuringExclusion[C]
