package countvotes.structures

import play.api.libs.json._
import play.api.libs.functional.syntax._


case class MajorityBonus(jackpot: Double, bonus: Double)

object MajorityBonus {

  implicit val majorityBonusReader: Reads[MajorityBonus] = (
    (__ \ "jackpot").read[Double] and
      (__ \ "bonus").read[Double]
    ) (MajorityBonus.apply _)

  implicit val majorityBonusWriter: Writes[MajorityBonus] = (
    (__ \ "jackpot").write[Double] and
      (__ \ "bonus").write[Double]
    ) (unlift(MajorityBonus.unapply))
}

case class ComparisonSets(set1: Array[String], set2: Array[String])

object ComparisonSets {

  implicit val comparisonSetsReader: Reads[ComparisonSets] = (
    (__ \ "set1").read[Array[String]] and
      (__ \ "set2").read[Array[String]]
    ) (ComparisonSets.apply _)


  implicit val comparisonSetsWriter: Writes[ComparisonSets] = (
    (__ \ "set1").write[Array[String]] and
      (__ \ "set2").write[Array[String]]
    ) (unlift(ComparisonSets.unapply))

}

case class MethodParam(comparisonOrder: Array[String], allowedVote: Int, cutOffQuota: Double,
                       proportionalRatio: Double, majorityBonus: MajorityBonus,
                       probabilityDistribution: Array[Double], comparisonSets: ComparisonSets)

object MethodParam {

  implicit val methodParamWriter: Writes[MethodParam] = (
    (__ \ "comparison_order").write[Array[String]] and
      (__ \ "allowed_vote").write[Int] and
      (__ \ "cut_off_quota").write[Double] and
      (__ \ "proportional_ratio").write[Double] and
      (__ \ "majority_bonus").write[MajorityBonus] and
      (__ \ "probability_distribution").write[Array[Double]] and
      (__ \ "comparison_sets").write[ComparisonSets]
    ) (unlift(MethodParam.unapply))

  implicit val methodParamReader: Reads[MethodParam] = (
    (__ \ "comparison_order").read[Array[String]] and
      (__ \ "allowed_vote").read[Int] and
      (__ \ "cut_off_quota").read[Double] and
      (__ \ "proportional_ratio").read[Double] and
      (__ \ "majority_bonus").read[MajorityBonus] and
      (__ \ "probability_distribution").read[Array[Double]] and
      (__ \ "comparison_sets").read[ComparisonSets]
    ) (MethodParam.apply _)
}





