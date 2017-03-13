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



class EVACSDelayedWDMethod extends ACT
 with TransferValueWithDenominatorWithNumOfMarkedContinuingBallotsOrOne
 with ACTSurplusDistribution
 with NoNewWinnersDuringSurplusesDistribution
 with NoNewWinnersDuringExclusion
 with ACTTotalsDuringExclusion
{
}
