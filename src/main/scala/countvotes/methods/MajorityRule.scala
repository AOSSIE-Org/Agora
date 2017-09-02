package countvotes.methods


import countvotes.structures._

import scala.collection.mutable.{HashMap => Map}

abstract class MajorityRule[B <: WeightedBallot with Weight] extends Scrutiny[B] {

}

