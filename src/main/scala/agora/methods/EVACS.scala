package agora.methods

import agora.structures._
import agora.algorithms._

import scala.collection.immutable.ListMap
import collection.mutable.{HashMap => Map}
import scala.collection.SortedMap
import collection.mutable.HashSet
import collection.breakOut
import scala.util.Random
import scala.util.Sorting
import java.io._

class EVACS extends ACT
 with TransferValueWithDenominatorWithNumOfMarkedContinuingBallotsOrOne
 with ACTSurplusDistribution
 with ACTNewWinnersDuringSurplusesDistribution
 with ACTNewWinnersDuringExclusion
 with ACTTotalsDuringExclusion
{
}
