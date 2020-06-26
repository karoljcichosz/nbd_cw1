import scala.collection.immutable.HashMap
import scala.collection.mutable

object Cw1 {
  val dniTygodnia = List("Poniedzialek", "Wtorek", "Środa", "Czwartek", "Piątek", "Sobota", "Niedziela")

  //-----------------------ZAD1------------------------------
  def stringFor(): String = {
    var string = "";
    for (i <- dniTygodnia) {
      string = string + i + ", "
    }
    string.dropRight(2)
  }

  def stringForP(): String = {
    var string = "";
    for (i <- dniTygodnia) {
      if (i(0) == 'P') {
        string = string + i + ", ";
      }
    }
    string.dropRight(2)
  }

  def stringWhile(): String = {
    var string = ""
    var i = 0
    while (i < dniTygodnia.size) {
      string = string + dniTygodnia(i) + ", "
      i += 1
    }
    string.dropRight(2)
  }

  //-----------------------ZAD2------------------------------
  def stringRecursion(list: List[String]): String = {
    if (list.isEmpty)
      ""
    else {
      if (list.size == 1)
        list.head
      else
        list.head + ", " + stringRecursion(list.tail)
    }
  }

  def stringRecursionReversed(list: List[String]): String = {
    if (list.isEmpty)
      ""
    else {
      if (list.size == 1)
        list.head
      else
        stringRecursionReversed(list.tail) + ", " + list.head
    }
  }

  //-----------------------ZAD3------------------------------
  @scala.annotation.tailrec
  def stringTailRecursion(list: List[String], string: String): String = {
    if (list.isEmpty) {
      return string;
    }
    var separator = ","
    if (string == "") {
      separator = ""
    }
    stringTailRecursion(list.tail, string + ", " + list.head)
  }

  //-----------------------ZAD4------------------------------
  def stringFoldl(): String = {
    dniTygodnia.foldLeft("") { (a, b) => a + ", " + b }.drop(2)
  }
  def stringFoldr(): String = {
    dniTygodnia.foldRight("") { (a, b) => a + ", " + b }.dropRight(2)
  }
  def stringFoldlP(): String = {
    val dniTygodniaFiltrowane = dniTygodnia filter(_(0) == 'P')
    dniTygodniaFiltrowane.foldLeft("") { (a, b) => a + ", " + b }.drop(2)
  }
  //-----------------------ZAD5------------------------------
  val products:HashMap[String,Double] = HashMap("kubek"->5.50, "dlugopis" -> 1.5, "olowek" -> 0.99)
  def discount(products: HashMap[String, Double]): HashMap[String, Double] = {
    products map { case (k, v) => (k, 0.9 * v) }
  }
  //-----------------------ZAD6------------------------------
  def krotka(tuple: (String, Int, Boolean)): Unit = {
    println(tuple._1)
    println(tuple._2)
    println(tuple._3)
  }
  //-----------------------ZAD7------------------------------

  def options = Map("a"->1,"b"->2)
  //-----------------------ZAD8------------------------------

  def deleteZeroes(list: List[Int]) : List[Int] = {
    if(list.isEmpty)
      list
    else if(list.head==0) deleteZeroes(list.tail)
    else list.head:: deleteZeroes(list.tail)
  }

  //-----------------------ZAD9------------------------------

  def mapOneMore(list: List[Int]) : List[Int] = {
    list map { case (x) => (x+1)}
  }
  //-----------------------ZAD9------------------------------
  def absoluteVals(list: List[Double]): List[Double] = {
    list.filter(x=> x>(-5) && x<12 ).map(x=>x.abs)
  }
  def main(): Unit = {
    println("zad1")
    println(stringFor())
    println(stringForP())
    println(stringWhile())
    println("zad2")
    println(stringRecursion(dniTygodnia))
    println(stringRecursionReversed(dniTygodnia))
    println("zad3")
    println(stringTailRecursion(dniTygodnia, ""))
    println("zad4")
    println(stringFoldl())
    println(stringFoldr())
    println(stringFoldlP())
    println("zad5")
    println(products)
    println(discount(products))
    println("zad6")
    krotka("string", 1, false)
    println("zad7")
    println(options.getOrElse("a","brak"))
    println(options.getOrElse("b","brak"))
    println(options.getOrElse("c","brak"))
    println("zad8")
    println(deleteZeroes(List(0,1,2,0,3,0,5,0)))
    println("zad9")
    println(mapOneMore(List(0,1,2,0,3,0,5,0)))
    println("zad10")
    println(absoluteVals(List(-10.0,-4.0,-3.0,0.0,1.0,6.0,11.0,14.0)))
  }

}
