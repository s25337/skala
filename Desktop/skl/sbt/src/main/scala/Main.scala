object Main extends App {
  // Zadanie 1: Zliczanie różnych znaków w napisie
  def countChars(str: String): Int = {
    str.distinct.length
  }

  // Zadanie 2: Zamiana elementów na parzystych i nieparzystych indeksach
  def swap[A](seq: Seq[A]): Seq[A] = {
    seq.grouped(2).flatMap {
      case Seq(a, b) => Seq(b, a)
      case Seq(a) => Seq(a)
      case _ => Seq.empty
    }.toSeq
  }

  // Zadanie 3: MasterMind - ocena ruchu
  def score(code: Seq[Int])(move: Seq[Int]): (Int, Int) = {
    val blackPegs = code.zip(move).count { case (c, m) => c == m }
    
    val codeRemaining = code.zip(move).collect { case (c, m) if c != m => c }
    val moveRemaining = code.zip(move).collect { case (c, m) if c != m => m }
    
    val whitePegs = moveRemaining.foldLeft((0, codeRemaining)) {
      case ((count, remaining), move) =>
        val idx = remaining.indexOf(move)
        if (idx >= 0) (count + 1, remaining.patch(idx, Nil, 1))
        else (count, remaining)
    }._1
    
    (blackPegs, whitePegs)
  }

  // Zadanie 4: Wyniki zawodów sportowych
  case class PartialScore(firstName: String, lastName: String, grace: Int, wit: Int)
  case class FinalResult(place: Int, firstName: String, lastName: String, graceAvg: Double, witAvg: Double, total: Double)

  def calculateResults(scores: List[PartialScore]): List[FinalResult] = {
    val groupedByContestant = scores.groupBy(s => (s.firstName, s.lastName))
    
    val averages = groupedByContestant.map { case ((firstName, lastName), contestantScores) =>
      val graceAvg = contestantScores.map(_.grace).sum.toDouble / contestantScores.size
      val witAvg = contestantScores.map(_.wit).sum.toDouble / contestantScores.size
      val total = graceAvg + witAvg
      (firstName, lastName, graceAvg, witAvg, total)
    }.toList
    
    val sorted = averages.sortBy { case (firstName, lastName, graceAvg, witAvg, total) =>
      (-total, -graceAvg, lastName, firstName)
    }
    
    val withPlaces = sorted.foldLeft((List.empty[FinalResult], 1, Option.empty[(Double, Double)])) {
      case ((results, currentPlace, prevScore), (firstName, lastName, graceAvg, witAvg, total)) =>
        val place = prevScore match {
          case Some((prevTotal, prevGrace)) if prevTotal == total && prevGrace == graceAvg =>
            results.head.place
          case _ => currentPlace
        }
        val result = FinalResult(place, firstName, lastName, graceAvg, witAvg, total)
        (result :: results, currentPlace + 1, Some((total, graceAvg)))
    }._1.reverse
    
    withPlaces
  }

  // Testy
  println("Zadanie 1:")
  println(s"countChars(\"hello\"): ${countChars("hello")}")
  
  println("\nZadanie 2:")
  println(s"swap(Seq(1, 2, 3, 4, 5)): ${swap(Seq(1, 2, 3, 4, 5))}")
  
  println("\nZadanie 3:")
  val code = Seq(1, 3, 2, 2, 4, 5)
  val move = Seq(2, 1, 2, 4, 7, 2)
  println(s"score($code)($move): ${score(code)(move)}")
  
  println("\nZadanie 4:")
  val testScores = List(
    PartialScore("Jan", "Kowalski", 15, 18),
    PartialScore("Jan", "Kowalski", 16, 17),
    PartialScore("Anna", "Nowak", 18, 19),
    PartialScore("Anna", "Nowak", 17, 18)
  )
  calculateResults(testScores).foreach(println)
}

/*/
Łukasz Mielewczyk
Kontakt
Dydaktyka
Pozostałe

Powrót (Programowanie obiektowo-funkcyjne)

Metody oferowane przez kolekcje
ćwiczenia laboratoryjne


Kolekcje (nowsze – wersja 3.1.0)
Iterable
Seq
Set
Map
String

Kolekcje (starsze – wersja 2.13.0)
Iterable
Seq
Set
Map
String

"Pętla" for/yield
for {
 stała1 <- kolekcja1
 stała2 <- kolekcja2
...
 stałaN <- kolekcjaN
} yield wyrażenie
dodanie po kolekcjach, wyrażenia if warunek – spowoduje pominięcie danych, które nie spełniają warunku
wyrażenie – wyrażenie według, którego dane są dodawane do listy


Zadania

Zadanie 1. Korzystając z metod oferowanych przez kolekcje zdefiniuj funkcję:
def countChars(str: String): Int = /* ... */
która zwróci ile jest różnych znaków użytych w napisie.

Zadanie 2. Korzystając z metod oferowanych przez kolekcje zdefiniuj funkcję:
def swap[A](seq: Seq[A]): Seq[A] = /* ... */
która zamieni kolejnością wartości znajdujących się na parzystych i nieparzystych indeksach.
Przykład:
Dla: seq = Seq(1, 2, 3, 4, 5), funkcja powinna zwrócić: Seq(2, 1, 4, 3, 5).

Zadanie 3. Gra MasterMind polega na odgadnięciu w jakich miejscach zostały umieszczone n ukrytych kul, które są oznaczone powtarzającymi się kolorami. Gracz wygrywa, jeżeli w określonej liczbie ruchów odgadnie w jakich miejscach zostały umieszczone kule. W każdej turze gracz wybiera n kul, po czym zostaje mu wyświetlona informacja czy trafił. Każda prawidłowo odgadnięta kula (kula o właściwym kolorze na właściwym miejscu) sygnalizowana jest czarną kropką. Natomiast jeżeli gracz odgadł kolor kuli, ale nie odgadł jej lokalizacji, jest to sygnalizowane białą kropką. Gracz nie wie, które kule są właściwe, które zaś nie.
Korzystając z funkcji kolekcji zdefiniuj funkcję:
def score(code: Seq[Int])(move: Seq[Int]): (Int, Int)
która ocenia ruchy dla gry MasterMind, czyli zwracającą liczbę czarnych i białych kropek.
Przykładowo, dla:
val code = Seq(1, 3, 2, 2, 4, 5)
val move = Seq(2, 1, 2, 4, 7, 2)
Funkcja powinna zwrócić: (1, 3).

Zadanie 4. Napisz program, który, przy pomocy metod oferowanych przez kolekcję, oblicza wyniki zawodów sportowych w konkurencji, w której zawodnicy oceniani są w dwóch kategoriach:
wdzięk
spryt
Ocena "cząstkowa" ma postać:
("Imię", "Nazwisko", ocena_wdzięku, ocena_sprytu)
Załóż, że:
zawodnicy identyfikowani są poprzez imię i nazwisko
każdy zawodnik może otrzymać dowolną liczbę ocen "cząstkowych"
ocena_wdzięku oraz ocena_sprytu są dowolnymi liczbami całkowitymi z zakresu od 0 do 20.
Ostateczny wynik zawodnika jest to para liczb typu Double będących średnimi arytmetycznymi ocen cząstkowych w podanych powyżej "kategoriach".
"Ranking" ustala się sumując obie "średnie" noty każdego z zawodników - wyższa suma oznacza lepszy wynik.
Jeśli sumy not dwóch zawodników są identyczne, to wyższe miejsce zajmuje ten, którego (średnia) nota za wdzięk była wyższa. Jeśli również noty za wdzięk są identyczne, to zawodnicy zajmują miejsca ex-aequo.
Załóż, że dane wejściowe programu stanowi lista obiektów reprezentujących oceny "cząstkowe". Program powinien stworzyć uporządkowaną listę obiektów reprezentujących informacje o:
miejscu zajętym przez zawodnika.
imieniu i nazwisku zawodnika.
uzyskanym wyniku.
W przypadku miejsc ex-aequo kolejność na liście wynikowej powinna być zgodna z porządkiem alfabetycznym nazwisk zawodników.
*/