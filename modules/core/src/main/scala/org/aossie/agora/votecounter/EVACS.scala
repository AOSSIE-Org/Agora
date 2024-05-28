package org.aossie.agora.votecounter

import org.aossie.agora.votecounter.stv._

class EVACS extends ACT
 with TransferValueWithDenominatorWithNumOfMarkedContinuingBallotsOrOne
 with ACTSurplusDistribution
 with ACTNewWinnersDuringSurplusesDistribution
 with ACTNewWinnersDuringExclusion
 with ACTTotalsDuringExclusion
