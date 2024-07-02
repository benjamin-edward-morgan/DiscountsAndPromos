package foo;    

import org.scalatest._
import flatspec._
import matchers._

class DiscountsSpec extends AnyFlatSpec with should.Matchers {

    "Discounts.getBestGroupPrices" should "work in the happy path case" in {
            val rates = Seq(
                Rate("M1", "Military"),
                Rate("M2", "Military"),
                Rate("S1", "Senior"),
                Rate("S2", "Senior"),
            )

            val cabinPrices = Seq(
                CabinPrice("CA", "M1", 200.00),
                CabinPrice("CA", "M2", 250.00),
                CabinPrice("CA", "S1", 225.00),
                CabinPrice("CA", "S2", 260.00),
                CabinPrice("CB", "M1", 230.00),
                CabinPrice("CB", "M2", 260.00),
                CabinPrice("CB", "S1", 245.00),
                CabinPrice("CB", "S2", 270.00),
            )

            val results = Discounts.getBestGroupPrices(rates, cabinPrices).toSet;

            val expectedResults = Set(
                BestGroupPrice("CA", "M1", 200.00, "Military"),
                BestGroupPrice("CA", "S1", 225.00, "Senior"),
                BestGroupPrice("CB", "M1", 230.00, "Military"),
                BestGroupPrice("CB", "S1", 245.00, "Senior"),
            )

            //make sure those two sets are equal
            assert(results.subsetOf(expectedResults) && expectedResults.subsetOf(results))
    } 

    "Discounts.getBestGroupPrices" should "ignore a CabinPrice with a rateCode not in the rates" in {
            //similar to above test but Senior discounts are excluded
            val rates = Seq(
                Rate("M1", "Military"),
                Rate("M2", "Military")
            )

            val cabinPrices = Seq(
                CabinPrice("CA", "M1", 200.00),
                CabinPrice("CA", "M2", 250.00),
                CabinPrice("CA", "S1", 225.00),
                CabinPrice("CA", "S2", 260.00),
                CabinPrice("CB", "M1", 230.00),
                CabinPrice("CB", "M2", 260.00),
                CabinPrice("CB", "S1", 245.00),
                CabinPrice("CB", "S2", 270.00),
            )

            val results = Discounts.getBestGroupPrices(rates, cabinPrices).toSet;

            //so the results only contain the matched rows
            val expectedResults = Set(
                BestGroupPrice("CA", "M1", 200.00, "Military"),
                BestGroupPrice("CB", "M1", 230.00, "Military"),
            )

            assert(results.subsetOf(expectedResults) && expectedResults.subsetOf(results))
    } 
}
