package org.aossie.agora.votecounter

import org.aossie.agora.model._
import org.aossie.agora.votecounter.stv._


import scala.collection.immutable.ListMap
import collection.mutable.{HashMap => Map}
import scala.collection.SortedMap
import collection.mutable.HashSet
import collection.breakOut
import scala.util.Random
import scala.util.Sorting
import java.io._

class EVACSnoLP extends ACT
 with TransferValueWithDenominatorWithNumOfAllContinuingBallotsOrOne // instead of TransferValueWithDenominatorWithNumOfMarkedContinuingBallotsOrOne
 with ACTVoteCounterWithAllContinuingBallotsInSurplusDistribution // instead  ACTSurplusDistribution
 with ACTNewWinnersDuringSurplusesDistribution
 with ACTNewWinnersDuringExclusion
 with ACTTotalsDuringExclusion
{
}
