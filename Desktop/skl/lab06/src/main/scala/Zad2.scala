@main
def zad2: Unit = {
  case class Województwo(nazwa: String, min: Int)
  // max ID gminy z województwa w: w.min + 19999

  case class Wynik(
    ID: Int,
    KOALICJA_OBYWATELSKA: Int,
    LEWICA_RAZEM: Int,
    POLEXIT: Int,
    JEDNOŚĆ_NARODU: Int,
    PIS: Int,
    EUROPA_CHRISTI: Int,
    WIOSNA: Int,
    KONFEDERACJA: Int,
    KUKIZ15: Int,
    POLSKA_FAIR_PLAY: Int
  )

  val województwa = List(
    Województwo("dolnośląskie",20000),
    Województwo("kujawsko-pomorskie",40000),
    Województwo("lubelskie",60000),
    Województwo("lubuskie",80000),
    Województwo("łódzkie",100000),
    Województwo("małopolskie",120000),
    Województwo("mazowieckie",140000),
    Województwo("opolskie",160000),
    Województwo("podkarpackie",180000),
    Województwo("podlaskie",200000),
    Województwo("pomorskie",220000),
    Województwo("śląskie",240000),
    Województwo("świętokrzyskie",260000),
    Województwo("warmińsko-mazurskie",280000),
    Województwo("wielkopolskie",300000),
    Województwo("zachodniopomorskie",320000)
  )

  val wyniki: List[Wynik] = io.Source
    .fromResource("wyniki.csv")
    .getLines
    .toList
    .map { l =>
      l.split(",").toList.map(_.toInt) match {
        case List(a,b,c,d,e,f,g,h,i,j,k) => Wynik(a,b,c,d,e,f,g,h,i,j,k)
        case _ => throw new IllegalArgumentException("Zła liczba kolumn w CSV")
      }
    }
  def totalVotes(w: Wynik): Int =
    w.KOALICJA_OBYWATELSKA +
    w.LEWICA_RAZEM +
    w.POLEXIT +
    w.JEDNOŚĆ_NARODU +
    w.PIS +
    w.EUROPA_CHRISTI +
    w.WIOSNA +
    w.KONFEDERACJA +
    w.KUKIZ15 +
    w.POLSKA_FAIR_PLAY

  case class StatWoj(
    woj: Województwo,
    procKO: Double,
    procPiS: Double,
    diff: Double
  )
  val statystyki: List[StatWoj] = województwa.map { w =>
    val minId = w.min
    val maxId = w.min + 19999
    val wierszeWoj = wyniki.filter(r => r.ID >= minId && r.ID <= maxId)
    val sumaKO   = wierszeWoj.map(_.KOALICJA_OBYWATELSKA).sum.toDouble
    val sumaPiS  = wierszeWoj.map(_.PIS).sum.toDouble
    val sumaAll  = wierszeWoj.map(totalVotes).sum.toDouble
    val (procKO, procPiS) =
      if (sumaAll == 0.0) (0.0, 0.0)
      else (
        sumaKO  * 100.0 / sumaAll,
        sumaPiS * 100.0 / sumaAll
      )

    val roznica = (procKO - procPiS).abs

    StatWoj(w, procKO, procPiS, roznica)
  }
  val minDiff = statystyki.map(_.diff).min
  val minimalne = statystyki.filter(s => (s.diff - minDiff).abs < 1e-9)
  println(f"Minimalna różnica procentowa KO vs PiS: $minDiff%.2f p.p.\n")
  minimalne.foreach { s =>
    println(s"Województwo: ${s.woj.nazwa}")
    println(f"  KO : ${s.procKO}%.2f %%")
    println(f"  PiS: ${s.procPiS}%.2f %%")
    println(f"  Różnica: ${s.diff}%.2f p.p.")
    println()
  }
}
