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

package countvotes.structures

import scala.language.implicitConversions

class ACTBallot(p: List[Candidate], override val id: Int,  m:Boolean, w: Rational, v: Rational)
extends MarkedWeightedBallot(p, id, m, w) with Value {
  val value = v
}

object ACTBallot {
  def apply(p: List[Candidate], id: Int, m:Boolean, w: Rational, v: Rational): ACTBallot = new ACTBallot(p,id,m,w, v)

  implicit def fromWeightedBallot(b: WeightedBallot): ACTBallot  = {
    new ACTBallot(b.preferences, b.id, true, b.weight, b.weight) // note that the marking is assigned true here
  }

}
