// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package countvotes.algorithms

import countvotes.structures._
import countvotes.methods._
import collection.mutable.{HashMap => Map}



trait NewWinnersOrderedByTotals[B <: Ballot with Weight] extends STV[B] with SurplusDistributionTieResolution{
  def returnNewWinners(totals: Map[Candidate, Rational], quota: Rational): List[(Candidate,Rational)] = {
    val ws = totals.clone().retain((k,v) => v >= quota)
    // val lws = ws.toSeq.sortWith(_._2 < _._2).toList
    resolveSurpluseDistributionTie(ws)
  }
}





trait NewWinnersNotOrdered[B <: Ballot with Weight] extends STV[B]{
  def returnNewWinners(totals: Map[Candidate, Rational], quota: Rational): List[(Candidate,Rational)] = {
    totals.clone().retain((k,v) => v >= quota).toList
  }
}
