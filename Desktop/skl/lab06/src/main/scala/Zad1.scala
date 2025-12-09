@main
def zad1: Unit = {
  val linie = io.Source
    .fromResource("ogniem-i-mieczem.txt")
    .getLines
    .toList

  def histogram(max: Int): String = {

    val tekst = linie.mkString(" ").toLowerCase
    val tylkoLitery = tekst.filter(_.isLetter)
    val mapa = tylkoLitery.groupBy(identity).view.mapValues(_.size).toMap
    val maxCount = if mapa.isEmpty then 1 else mapa.values.max
    val wiersze = mapa.toSeq
      .sortBy(_._1) // po literze
      .map { case (litera, liczba) =>
        val gwiazdki =
          if maxCount <= max then
            "*" * liczba
          else
            "*" * (liczba * max / maxCount)

        s"$litera:$gwiazdki"
      }

    wiersze.mkString("\n")
  }
  println(histogram(40)) 
}

