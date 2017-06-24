package countvotes.analysers

import countvotes.structures.{Candidate, Election, WeightedBallot}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by deepeshpandey on 23/06/17.
  */
object SinglePeakAnalyser extends PreferenceAnalysisMethod[WeightedBallot] {

  def analyse(e: Election[WeightedBallot], ccandidates: List[Candidate]): Unit = {


    def singlePeakRecursiveUtil(leftList: ListBuffer[Candidate], rightList: ListBuffer[Candidate]): List[Candidate] = {
      if ( leftList.size + rightList.size == ccandidates.size) {
        return (leftList ++ rightList).toList
      }

      val B = e.map(b => b.preferences.reverse).flatMap(l => (l.filter(c => !leftList.contains(c) && !rightList.contains(c))).take(1)).toSet

      // check if size of B is 1 and all candidates are placed in linear order
      if (B.size == 1 && (leftList ++ rightList).size == ccandidates.size - 1) {
        leftList.insert(leftList.size, B.head)
        return singlePeakRecursiveUtil(leftList, rightList)
      }

      if (B.size > 2 || B.isEmpty) {
        Nil
      } else {
        if (!(leftList.toList.isEmpty && rightList.toList.isEmpty)) {

          val l = leftList.toList.last
          val r = rightList.toList.head

          val L = e.map(b => b.preferences).map(p => B.toList.filter(x => p.reverse.filter(c => B.contains(c)).head == x && p.indexOf(r) < p.indexOf(x) && (p.indexOf(x) < p.indexOf(l)))).flatten.toSet
          val R = e.map(b => b.preferences).map(p => B.toList.filter(x => p.reverse.filter(c => B.contains(c)).head == x && p.indexOf(l) < p.indexOf(x) && (p.indexOf(x) < p.indexOf(r)))).flatten.toSet

          if (L.size < 2 && R.size < 2 && L.intersect(R).isEmpty) {
            L.size match {
              case 0 => if ((B -- R).nonEmpty) leftList.insert(leftList.size, (B -- R).head)
              case 1 => leftList.insert(leftList.toList.size, L.head)
            }

            R.size match {
              case 0 => (B -- L).size match {
                case 1 => if (L.nonEmpty) rightList.insert(0, (B -- L).head)
                case 2 => {
                  rightList.insert(0, (B -- L).tail.head)
                }
                case _ => {}
              }
              case 1 => rightList.insert(0, R.head)
            }
            singlePeakRecursiveUtil(leftList, rightList)
          } else {
            Nil
          }

        } else {
          if (B.size == 2) {
            leftList.insert(leftList.size, B.head)
            rightList.insert(0, B.last)
          } else {
            leftList.insert(0, B.head)
          }
          return singlePeakRecursiveUtil(leftList, rightList)
        }
      }
    }

    val singlePeakLinearOrder = singlePeakRecursiveUtil(ListBuffer(), ListBuffer())

    if (singlePeakLinearOrder.size == ccandidates.size) {
      println("Preference profile is single-peaked with respect to the following order.\n")
      println(singlePeakLinearOrder.mkString(" "))
    }
    else {
      println("Preference profile is not single peaked \n\n")

    }


  }
}
