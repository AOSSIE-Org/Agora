package org.aossie.agora.votecounter

import org.aossie.agora.model.Candidate
import org.aossie.agora.votecounter.stv._

class EVACS[C <: Candidate]
    extends ACT[C]
    with TransferValueWithDenominatorWithNumOfMarkedContinuingBallotsOrOne[C]
    with ACTSurplusDistribution[C]
    with ACTNewWinnersDuringSurplusesDistribution[C]
    with ACTNewWinnersDuringExclusion[C]
    with ACTTotalsDuringExclusion[C]
