package foo;

import scala.collection.mutable;

case class Promotion(code: String, notCombinableWith: Seq[String])
case class PromotionCombo(promotionCodes: Seq[String])

object Promos {
  
    //helper for below methods:
    //given some chosen set of preselected promo codes, 
    //and a remaining set of promotions, 
    //return a set of sets where inner sets are valid promo combos
    private def recursiveFindMorePromos(chosen: Set[String], remaining: Set[Promotion]): Set[Set[String]] = {
        //filter all the remaining promotions that are combinable with the chosen set
        val combineable = remaining.filterNot{ promo => 
            promo.notCombinableWith.exists(chosen.contains)
        }

        if(combineable.isEmpty) {
            //if no more promotions can be added, return the set we have chosen so far
            Set(chosen)
        } else {
            //otherwise combine the chosen set 
            combineable.flatMap{ promo => 
                //add this promo to the chosen set, remove this promo from the possible remaining promos
                recursiveFindMorePromos(chosen + promo.code, combineable - promo)    
            }
        }
    }

    /**
      * return all longest possible combinations of promos
      */
    def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
        recursiveFindMorePromos(Set.empty, allPromotions.toSet)
            .toSeq
            .map{ promoSet => 
                //returning each promo in sorted order
                PromotionCombo(promoSet.toSeq.sorted) 
            }
    }

    /**
      * return all longest possible combinations of promos that include promotionCode 
      * the supplied promotionCode is returned first in all returned results
      */
    def combinablePromotions(
        promotionCode: String,
        allPromotions: Seq[Promotion]
    ): Seq[PromotionCombo] = {
        recursiveFindMorePromos(Set(promotionCode), allPromotions.toSet.filterNot(_.code == promotionCode))
            .toSeq 
            .map{ promoSet => 
                //putting the provided promo first, sorting remaining
                PromotionCombo(promotionCode +: (promoSet - promotionCode).toSeq.sorted) 
            }
    }
}
