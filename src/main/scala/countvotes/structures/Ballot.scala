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

import scala.languageFeature.implicitConversions

class Ballot(val preferences: List[Candidate], val id: Int)

trait Weight extends Ballot {
  val weight: Rational
}

trait Value extends Ballot {
  val value: Rational
}

trait Marking extends Ballot {
  val marking: Boolean
}


class WeightedBallot(p: List[Candidate], id: Int, w: Rational)
extends Ballot(p, id) with Weight {
  val weight = w
  override def toString: String = "[" + id + ", " + p + ", " + w + "]"
}
object WeightedBallot {
  def apply(p: List[Candidate], id: Int, w: Rational): WeightedBallot  = new WeightedBallot(p, id, w)
  //def apply(p: List[Candidate], id: Int, w: Rational = new Rational(1,1)) = new SimpleBallot(p, id, w)
  //def apply(p: List[Candidate], id: Int) = new SimpleBallot(p, id, new Rational(1,1))
 // implicit def fromBallot(b: Ballot) = {
 //   new WeightedBallot(b.preferences, b.id, 1) // note that the weight is assigned 1 here
 // }
}


class MarkedWeightedBallot(p: List[Candidate], id: Int,  m: Boolean, w: Rational)
extends WeightedBallot(p, id, w) with Marking {
  val marking = m
}
object MarkedWeightedBallot{
  def apply(p: List[Candidate], id: Int, m:Boolean, w: Rational): MarkedWeightedBallot = new MarkedWeightedBallot(p, id, m, w)
 // implicit def fromWeightedBallot(b: WeightedBallot) = {
 //   new MarkedWeightedBallot(b.preferences, b.id, true, b.weight) // note that the marking is assigned true here
 // }
}
