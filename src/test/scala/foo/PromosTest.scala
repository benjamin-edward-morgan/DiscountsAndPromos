package foo;

import org.scalatest._
import flatspec._
import matchers._

class PromosTest extends AnyFlatSpec with should.Matchers {
  
    "Promos.allCombinablePromotions" should "work in the example case" in {
        val allPromos = Seq(
            Promotion("P1", Seq("P3")), // P1 is not combinable with P3
            Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
            Promotion("P3", Seq("P1")), // P3 is not combinable with P1
            Promotion("P4", Seq("P2")), // P4 is not combinable with P2
            Promotion("P5", Seq("P2")), // P5 is not combinable with P2
        );

        val allPromoCombos = Promos.allCombinablePromotions(allPromos);

        allPromoCombos.size shouldEqual 4
        Seq(
            Seq("P1", "P2"),
            Seq("P2", "P3"),
            Seq("P1", "P4", "P5"),
            Seq("P3", "P4", "P5"),
        ).foreach { combo => 
            allPromoCombos.contains(PromotionCombo(combo)) shouldBe true
        }

    }

    "Promos.allCombinablePromotions" should "return each promo indivudually if each is incompatible with all others" in {
        val allPromos = Seq(
            Promotion("P1", Seq("P2", "P3", "P4", "P5")),
            Promotion("P2", Seq("P1", "P3", "P4", "P5")), 
            Promotion("P3", Seq("P1", "P2", "P4", "P5")),
            Promotion("P4", Seq("P1", "P2", "P3", "P5")), 
            Promotion("P5", Seq("P1", "P2", "P3", "P4")),
        );

        val allPromoCombos = Promos.allCombinablePromotions(allPromos);

        allPromoCombos.size shouldEqual 5
        Seq(
            Seq("P1"),
            Seq("P2"),
            Seq("P3"),
            Seq("P4"),
            Seq("P5"),
        ).foreach { combo => 
            allPromoCombos.contains(PromotionCombo(combo)) shouldBe true
        }
    }

    "Promos.allCombinablePromotions" should "return all promos in a single group if there are no incompatabilities" in {
        val allPromos = Seq(
            Promotion("P1", Nil),
            Promotion("P2", Nil), 
            Promotion("P3", Nil),
            Promotion("P4", Nil), 
            Promotion("P5", Nil),
        );

        val allPromoCombos = Promos.allCombinablePromotions(allPromos);

        allPromoCombos shouldEqual Seq(
            PromotionCombo(Seq("P1", "P2", "P3", "P4", "P5"))
        )  
    }

    "Promos.combinablePromotions" should "work in the example case" in {
        val allPromos = Seq(
            Promotion("P1", Seq("P3")), // P1 is not combinable with P3
            Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
            Promotion("P3", Seq("P1")), // P3 is not combinable with P1
            Promotion("P4", Seq("P2")), // P4 is not combinable with P2
            Promotion("P5", Seq("P2")), // P5 is not combinable with P2
        );

        val p3PromoCombos = Promos.combinablePromotions("P3", allPromos);

        p3PromoCombos.size shouldEqual 2 
        Seq(
            Seq("P3", "P2"),
            Seq("P3", "P4", "P5"),
        ).foreach { combo => 
            p3PromoCombos.contains(PromotionCombo(combo)) shouldBe true
        }
    }
}
