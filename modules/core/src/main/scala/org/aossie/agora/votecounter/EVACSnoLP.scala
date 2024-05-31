package org.aossie.agora.votecounter

import org.aossie.agora.votecounter.stv._

class EVACSnoLP
    extends ACT
    with TransferValueWithDenominatorWithNumOfAllContinuingBallotsOrOne // instead of TransferValueWithDenominatorWithNumOfMarkedContinuingBallotsOrOne
    with ACTVoteCounterWithAllContinuingBallotsInSurplusDistribution    // instead  ACTSurplusDistribution
    with ACTNewWinnersDuringSurplusesDistribution
    with ACTNewWinnersDuringExclusion
    with ACTTotalsDuringExclusion {}
