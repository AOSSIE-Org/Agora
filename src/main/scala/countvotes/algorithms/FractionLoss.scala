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


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// ACT
// This function takes a lot of time. The running time is large because of it.

trait ACTFractionLoss extends STV[ACTBallot]{

  def loseFraction(e: Election[ACTBallot], ccandidates: List[Candidate]): Election[ACTBallot] = {
    val pt = totals(e,ccandidates)
    var newe = e
    for ((k,v) <- pt) {
      val n = BigDecimal(v.numerator / v.denominator).setScale(0, BigDecimal.RoundingMode.DOWN).toInt
      //println("k: " + k + "; v: " + v + "; n: " + n)
      val neweste = for (b <- newe if !b.preferences.isEmpty) yield {
        if (b.preferences.head == k) {
          if (v.numerator != 0) {
            ACTBallot(b.preferences, b.id, b.marking, b.weight * (n / v), b.value )
          }
          else {
            ACTBallot(b.preferences, b.id, b.marking, Rational(0,1),  Rational(0,1))
          }
        }
        else {
          b
        }
      }
      newe = neweste
    }
   newe
  }

}


trait NoFractionLoss extends STV[ACTBallot]{

  def loseFraction(e: Election[ACTBallot], ccandidates: List[Candidate]): Election[ACTBallot] = {
    e
  }

}
