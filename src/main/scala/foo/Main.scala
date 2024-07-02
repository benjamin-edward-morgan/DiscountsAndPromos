package foo;


object Main extends App {

    //demo Problem 1 
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

    val bestCabinPrices = Discounts.getBestGroupPrices(rates, cabinPrices);
    println(
        s"""Input - Rates
        |  ${rates.mkString("\n  ")}
        |Input - Cabin Prices
        |  ${cabinPrices.mkString("\n  ")}
        |Output - Best Cabin Prices
        |  ${bestCabinPrices.mkString("\n  ")}
        |""".stripMargin
    )

    //demo Problem 2 
    val allPromos = Seq(
        Promotion("P1", Seq("P3")), // P1 is not combinable with P3
        Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
        Promotion("P3", Seq("P1")), // P3 is not combinable with P1
        Promotion("P4", Seq("P2")), // P4 is not combinable with P2
        Promotion("P5", Seq("P2")), // P5 is not combinable with P2
    );

    val allPromoCombos = Promos.allCombinablePromotions(allPromos);
    val promoCombosP1 = Promos.combinablePromotions("P1", allPromos);
    val promoCombosP3 = Promos.combinablePromotions("P3", allPromos);

    println(
        s"""Input - Promotions
        |  ${allPromos.mkString("\n  ")}
        |Output - All Promo Combos
        |  ${allPromoCombos.mkString("\n  ")}
        |Output - Promotion Combinations for promotionCode=”P1”
        |  ${promoCombosP1.mkString("\n  ")}
        |Output - Promotion Combinations for promotionCode="P3"
        |  ${promoCombosP3.mkString("\n  ")}
        |""".stripMargin
    )


}