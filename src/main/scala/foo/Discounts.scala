package foo;

import scala.collection.mutable;

case class Rate(
    rateCode: String, 
    rateGroup: String
)

case class CabinPrice(
    cabinCode: String,
    rateCode: String,
    price: BigDecimal
)

case class BestGroupPrice(
    cabinCode: String,
    rateCode: String,
    price: BigDecimal,
    rateGroup: String
)

object Discounts {

    //this is used as key for the HashMap below, and represents a disting cabinCode/rateGroup combo
    private case class CabinGroup(
        cabinCode: String,
        rateGroup: String,
    )
    
    /**
      * Returns the best group prices by rateGroup/cabinCode given a list of Rates and CabinPrices 
      * 
      * If the CabinPrice has a rateCode not found in rates, it will be excluded from the results
      */
    def getBestGroupPrices(
        rates: Seq[Rate],
        prices: Seq[CabinPrice]
    ): Seq[BestGroupPrice] = {

        //create a map of code -> group for faster lookup
        val ratesMap = rates.map(rate => {
            (rate.rateCode, rate.rateGroup)
        }).toMap

        //store the current best price for each Cabin/RateGroup combo
        val bestCabinPrices = mutable.HashMap[CabinGroup, BestGroupPrice]();

        //iterate through the prices, look up a rate code
        for { 
            price <- prices 
            rateGroup <- ratesMap.get(price.rateCode).toSeq
        } {
            //construct this price and CabinGroup key
            val cabinGroup = CabinGroup(price.cabinCode, rateGroup);
            val thisPrice =  BestGroupPrice(
                cabinCode = price.cabinCode,
                rateCode = price.rateCode,
                price = price.price,
                rateGroup = rateGroup,
            )
            bestCabinPrices.get(cabinGroup) match {
                case None => {
                    //no price for this cabinGroup yet, so this is the best for now
                    bestCabinPrices.put(cabinGroup, thisPrice)
                }
                case Some(lastBestPrice) if lastBestPrice.price > price.price => {
                    //this price beats the existing best price, so update it 
                    bestCabinPrices.put(cabinGroup, thisPrice)
                }
                case _ => {} //otherwise do no update
            }
        }

        //return the best cabin prices in no particular order
        bestCabinPrices.values.toSeq
    }
}
