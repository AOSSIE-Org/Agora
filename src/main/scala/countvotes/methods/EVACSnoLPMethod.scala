package countvotes.methods

import countvotes.structures._
import countvotes.algorithms._


import scala.collection.immutable.ListMap
import collection.mutable.{HashMap => Map}
import scala.collection.SortedMap
import collection.mutable.HashSet
import collection.breakOut
import scala.util.Random
import scala.util.Sorting
import java.io._

class EVACSnoLPMethod extends ACT
 with TransferValueWithDenominatorWithNumOfAllContinuingBallotsOrOne // instead of TransferValueWithDenominatorWithNumOfMarkedContinuingBallotsOrOne
 with ACTScrutinyWithAllContinuingBallotsInSurplusDistribution // instead  ACTSurplusDistribution
 with ACTNewWinnersDuringSurplusesDistribution
 with ACTNewWinnersDuringExclusion
 with ACTTotalsDuringExclusion
{
}
