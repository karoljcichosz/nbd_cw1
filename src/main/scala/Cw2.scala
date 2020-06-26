object Cw2 {
  //-----------------------ZAD1------------------------------
  val dniPracujace = List("Poniedzialek", "Wtorek", "Środa", "Czwartek", "Piątek")
  val dniWeekend = List("Sobota", "Niedziela")

  def dayResolver(day: String): String = day match {
    case a if dniPracujace.contains(day) => "Praca"
    case b if dniWeekend.contains(day) => "Weekend"
    case c => "Nie ma takiego dnia"
  }

  //-----------------------ZAD2------------------------------
  class KontoBankowe() {
    private var stanKonta = 0;

    def this(stanKonta: Int) {
      this()
      this.stanKonta = stanKonta
    }

    def getStanKonta(): Int = stanKonta

    def wplata(ile: Int) = stanKonta = stanKonta + ile

    def wyplata(ile: Int) = stanKonta = stanKonta - ile
  }

  //-----------------------ZAD3------------------------------
  case class Osoba(imie: String, nazwisko: String) {}

  def message(osoba: Osoba): String = osoba.imie match {
    case "Karol" => "hello Karol"
    case "Ania" => "hi Ania"
    case default => "Welcome " + osoba.imie + " " + osoba.nazwisko
  }
  //-----------------------ZAD3------------------------------
  def twoParameters(int: Int, fun: Int=>Int): Int = {
    fun(fun(fun(int)))
  }
  def testFun(int: Int):Int = {
    int+int
  }
  //-----------------------ZAD4------------------------------
  class Osoba2(private var _imie: String, private var _nazwisko: String) {
    private val _podatek: Double = 0
    def podatek: Double = _podatek
    def imie: String = _imie
    def nazwisko: String = _nazwisko
    def info(): Unit = {
      println(this.imie + " " + this.nazwisko + ", podatek: " + this.podatek)
    }
  }

  trait Student extends Osoba2 {
    override def podatek: Double = 0
  }

  trait Nauczyciel extends Pracownik {
    override def podatek: Double = getPensja() * 0.1
  }

  trait Pracownik extends Osoba2 {
    private var pensja: Int = 0
    def setPensja(p:Int) = pensja=p;
    def getPensja() = pensja
    override def podatek: Double = pensja * 0.2
  }



  def test5(): Unit ={
    val osoba1 = new Osoba2("Karol", "Cichosz") with Pracownik;
    osoba1.setPensja(2500)
    osoba1.info()

    val osoba2 = new Osoba2("Adam", "Malysz") with Nauczyciel;
    osoba2.setPensja(9000)
    osoba2.info()

    val osoba3 = new Osoba2("Anna", "Annowska") with Student;
    osoba3.info();

    val osoba4 = new Osoba2("Sebastian", "Sebowski") with Student with Pracownik;
    osoba4.setPensja(20500)
    osoba4.info()

    val osoba5 = new Osoba2("Artur", "Krol") with Pracownik with Student;
    osoba5.setPensja(20500)
    osoba5.info()
  }


  def main(): Unit = {
    println("zad2.1")
    println(dayResolver("Poniedzialek"))
    println(dayResolver("Sobota"))
    println(dayResolver("Szustek"))
    println("zad2.2")
    val konto1 = new KontoBankowe()
    println(konto1.getStanKonta())
    konto1.wplata(500)
    println(konto1.getStanKonta())

    val konto2 = new KontoBankowe(1000)
    println(konto2.getStanKonta())
    konto2.wyplata(500)
    println(konto2.getStanKonta())

    println("zad2.3")
    val osoba1 = Osoba("Karol", "Cichosz")
    val osoba2 = Osoba("Ania", "Biernacka")
    val osoba3 = Osoba("Kuba", "Knap")

    println(message(osoba1))
    println(message(osoba2))
    println(message(osoba3))

    println("zad2.4")
    println(twoParameters(3,testFun))

    println("zad2.5")
    test5()
  }
}
