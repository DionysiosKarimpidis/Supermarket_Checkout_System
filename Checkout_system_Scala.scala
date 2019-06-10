object main extends App {

  import scala.math.Integral.Implicits._

  //dictionaries
  var products = Map("a"-> 50,"b"-> 30, "c" -> 20, "d" -> 15)
  var special_products = Map("a"-> 3,"b"-> 2)
  var special_price = Map("a"-> 130,"b"-> 45)

  var basket = Array[String]()

  main()

  def main(): Unit = {
    checkout(Array("a", "b", "b", "c"))   //change manualy
  }

  def checkout(purchased_items: Array[String]){

    for (item <- purchased_items) {
      scan_item(item)
    }

    var total = 0
    var tmp_normal = 0
    var tmp_special = 0

    var unique = basket.distinct  //set of products in basket
    for (i <- 0 to unique.length-1){  //iterate on each set
      var quantity = basket.groupBy(identity).mapValues(_.size)(unique(i))  //quantity of each product in basket
      if (is_special(unique(i), quantity)){
        tmp_special += calculate_special(unique(i), quantity)   //calculate special price
      } else {
        tmp_normal += caclulate_normal(unique(i), quantity )  //calculate normal price
      }
    }
    total = tmp_special + tmp_normal
    print("Total value of products",total)

  }


  def scan_item(item: String){
    if (products.contains(item)){   //product exists in catalog
      basket :+= item
    } else{
      println("No such product. Calculating the rest")
    }
  }


  def calculate_special(item: String, q: Int): Int ={

    var number_of_special_needed = special_products.get(item).get
    var product_special_price = special_price.get(item).get
    var normal_price = products.get(item).get

    var (div, reminder) = q /% number_of_special_needed
    return ((div * product_special_price) + (reminder * normal_price))
  }


  def caclulate_normal(item: String, q: Int): Int ={
    return q * products.get(item).get

  }


  def is_special(item: String, q: Int): Boolean ={

    if (special_products.contains(item)){
      var number_of_secial_needed = special_products.get(item).get
      var (div, reminder) = q /% number_of_secial_needed
      if (div >= 1) true
      else false
    }
    else
      false

  }




}
