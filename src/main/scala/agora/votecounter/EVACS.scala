package agora.votecounter

import agora.votecounter.stv._

class EVACS
    extends ACT
    with TransferValueWithDenominatorWithNumOfMarkedContinuingBallotsOrOne
    with ACTSurplusDistribution
    with ACTNewWinnersDuringSurplusesDistribution
    with ACTNewWinnersDuringExclusion
    with ACTTotalsDuringExclusion
