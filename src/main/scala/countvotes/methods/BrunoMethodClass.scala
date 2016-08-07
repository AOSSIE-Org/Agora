package countvotes.methods

import countvotes.structures._
import countvotes.algorithms._

abstract class BrunoMethodClass[B <: Ballot with Weight] extends VoteCountingMethod[B] {

  val report: Report[B] = new Report[B]

}
