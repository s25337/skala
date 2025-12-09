# Programowanie Obiektowo-Funkcyjne w Scali
## Kompletny zbiór laboratoriów

---

## LABORATORIUM 1

### Zadanie 1: Liczby pierwsze i hipoteza Goldbacha

**Opis:** Program znajduje wszystkie liczby pierwsze mniejsze od podanej liczby n, a następnie dla każdej parzystej liczby od 4 do n znajduje pierwsze dwie liczby pierwsze, których suma daje tę liczbę parzystą (hipoteza Goldbacha).

**Kod:**
```scala
@main
def z1: Unit = {
  val n = promptPositiveInt() // gets positive integer

  val primes = Range(2, n).filter {                       // creates range [2, n), because 0 + n = n, but 0 is not prime, so [2, n] is not needed
    case x if x < 2       => false                        // if number < 2, not prime
    case 2                => true                         // if number == 2, is prime
    case x if x % 2 == 0  => false                        // if number is divisible by 0, not prime
    case x =>                                             // all other numbers - get a range [2, sqrt(x)] of only odd numbers which are all possible dividers
      !(3 to math.sqrt(x).toInt by 2).exists(x % _ == 0)  // check if range has a divider that will result in no remainder - if yes, negation return false, so not prime
  }
  val primePairs = for {  // for comprehension
    p1 <- primes          // first iteration of for loop
    p2 <- primes          // second iteration of for loop, loop in a loop
    if p1 <= p2           // only if first prime is less or equal to second, gets rid of "duplicate" tuples, like (2, 3) and (3, 2)
  } yield (p1, p2)        // creates tuple (prime 1, prime 2) on every loop in loop iteration

  val sums = Range.inclusive(4, n, 2).flatMap { x =>  // creates range (2, n] of even numbers, but actually [4, n] of evens, because 3 can be skipped, then iterates over it
    primePairs.flatMap { case (p1, p2) =>             // iterates over non-duplicate pairs of primes
      if (p1 + p2 == x) Some((p1, p2, x)) else None   // if sum of primes equals even number x, returns data
    }                                                 // flatMap "purifies" generated Vector of optionals, so Vector(Some(data), Some(data), None) => Vector(data, data)
  }.groupBy(_._3)                                     // group by third element of tuples inside the vector, so that is by the result of summing primes, putting tuples inside another sequence
    .values                                           // get list of grouped tuples, keys are discarded and resulting items are sequences of many tuples where each group has same sum result
    .flatMap(_.headOption)                            // flats the groups into one big sequence, taking only the first element of each group
    .toList
    .sortBy(_._3)                                     // sort by third position in the tuples, so by the sum result

  for {
    (p1, p2, r) <- sums
  } yield println(s"$p1 + $p2 = $r") // prints out the sums
}

def promptPositiveInt(): Int = {
  while (true) {
    print("> n=")

    try {
      val number = io.StdIn.readInt() // get int from user

      if (number <= 0) // throw illegal argument if number is not positive
        throw new IllegalArgumentException() 

      return number
    } catch {
      case _: NumberFormatException |     // not a number
           _: IllegalArgumentException => // not a positive number
        println("Number must be a positive integer")
    }
  }

  return 1 // if somehow we break out of the loop, return 1 for safety
}
```

**Kluczowe koncepcje:**
- Filtrowanie zakresów (Range)
- Pattern matching
- For comprehension
- flatMap i groupBy
- Optionals (Some/None)

---

### Zadanie 2: Liczby zespolone

**Opis:** Implementacja klasy liczb zespolonych C z pełnym wsparciem operacji arytmetycznych (+, -, *, /) oraz porównań (<, >, <=, >=, ==, !=). Umożliwia działania typu: Complex + Complex, Complex + Double, Double + Complex.

**Kod:**
```scala
@main
def z2: Unit = {
  val x1 = C(-2, -4)
  val x2 = C(5, 1)
  val x3 = C(8)
  val x4 = C(0, -2)

  println(s"x1 = $x1")
  println(s"x2 = $x2")
  println(s"x3 = $x3")
  println(s"x4 = $x4")

  println(s"x1 +  x2 =  ${x1 + x2}")
  println(s"x1 +  5  =  ${x1 + 5.0}")
  println(s"5  +  x1 =  ${5.0 + x1}")
  println(s"x1 -  x2 =  ${x1 - x2}")
  println(s"x1 -  5  =  ${x1 - 5.0}")
  println(s"5  -  x1 =  ${5.0 - x1}")
  println(s"x1 *  x2 =  ${x1 * x2}")
  println(s"x1 *  5  =  ${x1 * 5.0}")
  println(s"5  *  x2 =  ${5.0 * x1}")
  println(s"x1 /  x2 =  ${x1 / x2}")
  println(s"x1 /  5  =  ${x1 / 5.0}")
  println(s"5  /  x2 =  ${5.0 / x1}")
  println(s"x1 == x2 =  ${x1 == x2}")
  println(s"x1 != x2 =  ${x1 != x2}")
  println(s"x1 <  x2 =  ${x1 < x2}")
  println(s"x1 >  x2 =  ${x1 > x2}")
  println(s"x1 <= x2 =  ${x1 <= x2}")
  println(s"x1 >= x2 =  ${x1 >= x2}")
}

case class C(val re: Double, val im: Double) {  // case class implements equals and hash methods automatically
  def this(re: Double) = {                      // secondary constructor for when imaginary = 0
    this(re, 0)
  }

  override def toString: String = im match {  // toString override for usage in `println( C(1, 2) )`
    case x if x > 0 => s"$re + ${im}i"        // + when imaginary is positive
    case x if x < 0 => s"$re - ${im.abs}i"    // - when imaginary is negative
    case _          => s"$re"                 // bare when imaginary is 0
  }

  def magnitude: Double = Math.sqrt(Math.pow(re, 2) + Math.pow(im, 2))              // magnitude in context of complex plane (cartesian plane, but x=Re and y=Im)

  def +(that: C): C = C(this.re + that.re, this.im + that.im)                       // Complex + Complex
  
  def +(scalar: Double): C = C(this.re + scalar, this.im)                           // Complex + Double

  def -(that: C): C = C(this.re - that.re, this.im - that.im)                       // Complex - Complex

  def -(scalar: Double): C = C(this.re - scalar, this.im)                           // Complex - Double

  def *(that: C): C =                                                               // Complex * Complex
    val nRe = this.re * that.re - this.im * that.im
    val nIm = this.im * that.re + this.re * that.im
    C(nRe, nIm)
  
  def *(scalar: Double): C = C(this.re * scalar, this.im * scalar)                  // Complex * Double

  def /(that: C): C = that match {                                                  // Complex / Complex
    case C(0, 0) => throw new IllegalArgumentException(s"Cannot divide by 0 + 0i")
    case _ =>
      val den = Math.pow(that.re, 2) + Math.pow(that.im, 2)
      val nRe = (this.re * that.re + this.im * that.im) / den
      val nIm = (this.im * that.re - this.re * that.im) / den
      C(nRe, nIm)
  }

  def /(scalar: Double): C = scalar match {                                         // Complex / Double
    case 0 => throw new IllegalArgumentException(s"Cannot divide by 0")
    case _ => C(this.re / scalar, this.im / scalar)
  }

  def ==(that: C): Boolean = this.equals(that)                // usage of `.equals()` included by defining `case class` rather than bare `class`
  
  def !=(that: C): Boolean = !(this == that)
  
  def <(that: C): Boolean = this.magnitude < that.magnitude   // checks against magnitude as per requirements in exercise description
  
  def >(that: C): Boolean = this.magnitude > that.magnitude
  
  def <=(that: C): Boolean = this.magnitude <= that.magnitude
  
  def >=(that: C): Boolean = this.magnitude >= that.magnitude
}

object C {                                              // allows for `val x = C(1, 2)` rather than `val x = new C(1, 2)`
  def apply(re: Double, im: Double): C = new C(re, im)  // primary constructor "override"
  def apply(re: Double): C = new C(re)                  // secondary constructor "override"
}

extension (double: Double)        // could also be done in `object C ... def +(scalar: Double, complex: C): C = ...` and so on for all arithmetic operations
  def +(c: C): C = C(double) + c  // allows for Double + Complex by transforming Double into Complex with no imaginary part
  def -(c: C): C = C(double) - c  // allows for Double - Complex by transforming Double into Complex with no imaginary part
  def *(c: C): C = C(double) * c  // look in `def *(that: C): ...`, this.re=double, this.im=0, so nRe=(double * that.re - 0 * that.im), nIm=(0 * that.re + double * that.im)
  def /(c: C): C = C(double) / c  // same as ^ above, [(double * re) / den] + [(-double * im) / den] i
```

**Kluczowe koncepcje:**
- Case class
- Konstruktory (główny i pomocniczy)
- Przeciążanie operatorów
- Pattern matching
- Extension methods (Scala 3)
- toString override

---

## LABORATORIUM 2: Rekurencja ogonowa (tail recursion)

### Zadanie 1: Odwracanie stringa

**Opis:** Funkcja rekurencyjna odwracająca ciąg znaków używając rekurencji ogonowej.

**Kod:**
```scala
import scala.annotation.tailrec

def reverse(str: String): String = {
  @tailrec
  def reverseHelper(remaining: String, acc: String): String = {
    if (remaining.isEmpty) acc
    else reverseHelper(remaining.tail, remaining.head +: acc)
  }
  reverseHelper(str, "")
}

@main
def l2z1(): Unit = {
  val input = scala.io.StdIn.readLine("Enter a string: ")
  val reversed = reverse(input)
  println(s"Reversed string: $reversed")
}
```

**Kluczowe koncepcje:**
- Rekurencja ogonowa (@tailrec)
- Akumulator
- String operations (head, tail, isEmpty)

---

### Zadanie 2: Liczby pierwsze

**Opis:** Funkcja sprawdzająca czy liczba jest pierwsza, używając rekurencji ogonowej.

**Kod:**
```scala
import scala.annotation.tailrec

def isPrime(n: Int): Boolean = {
  if (n < 2) false
  else {
    @tailrec
    def loop(divisor: Int): Boolean = {
      if (divisor * divisor > n) true
      else if (n % divisor == 0) false
      else loop(divisor + 1)
    }

    loop(2)
  }
}

@main
def l2z2(): Unit = {
  println("Enter a positive integer n: ")
  val x = scala.io.StdIn.readInt()
  val primes = (2 until x).filter(isPrime)
  println(s"Prime numbers less than $x: ${primes.mkString(", ")}")
}
```

**Kluczowe koncepcje:**
- Rekurencja ogonowa
- Algorytm sprawdzania pierwszości
- Filter na Range

---

### Zadanie 3: Konwersja binarny → dziesiętny

**Opis:** Konwertuje liczbę binarną (jako Int) na liczbę dziesiętną używając rekurencji ogonowej.

**Kod:**
```scala
import scala.annotation.tailrec

def binToDec(bin: Int): Int = {
  @tailrec
  def loop(remaining: Int, power: Int, acc: Int): Int = {
    if (remaining == 0) acc
    else {
      val bit = remaining % 10        // ostatnia cyfra binarna (0 lub 1)
      val value = bit * math.pow(2, power).toInt
      loop(remaining / 10, power + 1, acc + value)
    }
  }

  loop(bin, 0, 0)
}

@main
def l2z3(): Unit = {
  println("Enter a binary number: ")
  val bin = scala.io.StdIn.readInt()
  val dec = binToDec(bin)
  println(s"Decimal value: $dec")
}
```

**Kluczowe koncepcje:**
- Rekurencja ogonowa z wieloma parametrami
- Operacje arytmetyczne (%, /)
- Potęgowanie (math.pow)

---

### Zadanie 4: Ciąg typu Fibonacciego

**Opis:** Oblicza wartość ciągu F(0)=2, F(1)=1, F(n)=F(n-1)+F(n-2) dla n>1, używając rekurencji ogonowej.

**Kod:**
```scala
import scala.annotation.tailrec

def value(n: Int): Int = {
  @tailrec
  def loop(i: Int, prev: Int, curr: Int): Int = {
    if (i == n) prev
    else loop(i + 1, curr, prev + curr)
  }

  if (n == 0) 2
  else if (n == 1) 1
  else loop(1, 1, 3)  // startujemy od F(1)=1, F(2)=3
}

@main
def l2z4(): Unit = {
  println(s"First 10 values: ${(0 until 10).map(value).mkString(", ")}")
}
```

**Kluczowe koncepcje:**
- Rekurencja ogonowa dla ciągów
- Dwa akumulatory (prev, curr)
- Pattern matching na wartościach początkowych

---

### Zadanie 5: Sprawdzanie uporządkowania tablicy

**Opis:** Sprawdza czy elementy tablicy są uporządkowane według danego predykatu.

**Kod:**
```scala
import scala.annotation.tailrec

def isOrdered(tab: Array[Int], mlr: (Int, Int) => Boolean): Boolean = {
  @tailrec
  def loop(index: Int): Boolean = {
    if (index >= tab.length - 1) true
    else if (!mlr(tab(index), tab(index + 1))) false
    else loop(index + 1)
  }

  loop(0)
}

@main
def main(): Unit = {
  val tab1 = Array(1, 3, 3, 6, 8)
  val tab2 = Array(1, 3, 3, 6, 8)

  println(isOrdered(tab1, _ <= _)) // true
  println(isOrdered(tab2, _ < _))  // false
}
```

**Kluczowe koncepcje:**
- Funkcje wyższego rzędu (predykat jako parametr)
- Rekurencja ogonowa z indeksem
- Anonymous functions (_ <= _)

---

### Zadanie 6: Wartość dwóch tablic z predykatem

**Opis:** Znajduje pierwszy indeks gdzie predykat jest spełniony dla elementów z dwóch tablic, a następnie aplikuje operację.

**Kod:**
```scala
import scala.annotation.tailrec

def worth(tab1: Array[Int], tab2: Array[Int])(pred: (Int, Int) => Boolean)(op: (Int, Int) => Int): Option[Int] = {
  @tailrec
  def loop(index: Int): Option[Int] = {
    if (index >= tab1.length || index >= tab2.length) None
    else if (pred(tab1(index), tab2(index))) Some(op(tab1(index), tab2(index)))
    else loop(index + 1)
  }

  loop(0)
}

@main
def l2z6(): Unit = {
  val tab1 = Array(-1, 3, 2, -8, 5)
  val tab2 = Array(-3, 3, 3, 0, -4, 5)          
  val result = worth(tab1, tab2)(_ < _)(_ + _)    
  println(result) // Should print Some(5)
}
```

**Kluczowe koncepcje:**
- Currying (wielokrotne listy parametrów)
- Option type (Some/None)
- Funkcje wyższego rzędu

---

## LABORATORIUM 3: Listy i pattern matching

### Pełny kod z wszystkimi 5 zadaniami:

```scala
import scala.annotation.tailrec

object Main {
  
  // Zadanie 1: Divide - podziel listę na elementy parzyste i nieparzyste indeksy
  def divide[A](list: List[A]): (List[A], List[A]) = {
    @tailrec
    def divideHelper(remaining: List[A], even: List[A], odd: List[A], isEven: Boolean): (List[A], List[A]) = {
      remaining match {
        case Nil => (even.reverse, odd.reverse)
        case head :: tail =>
          if (isEven) divideHelper(tail, head :: even, odd, false)
          else divideHelper(tail, even, head :: odd, true)
      }
    }
    divideHelper(list, List.empty[A], List.empty[A], true)
  }
  
  // Zadanie 2: Merge - scal dwie posortowane listy
  def merge[A](a: List[A], b: List[A])(leq: (A, A) => Boolean): List[A] = {
    @tailrec
    def mergeHelper(list1: List[A], list2: List[A], acc: List[A]): List[A] = {
      (list1, list2) match {
        case (Nil, Nil) => acc.reverse
        case (Nil, _) => acc.reverse ++ list2
        case (_, Nil) => acc.reverse ++ list1
        case (h1 :: t1, h2 :: t2) =>
          if (leq(h1, h2)) mergeHelper(t1, list2, h1 :: acc)
          else mergeHelper(list1, t2, h2 :: acc)
      }
    }
    mergeHelper(a, b, List.empty[A])
  }
  
  // Zadanie 3: Compress - zamień powtarzające się elementy na pary (element, liczba)
  def compress[A](list: List[A]): List[(A, Int)] = {
    @tailrec
    def compressHelper(remaining: List[A], acc: List[(A, Int)]): List[(A, Int)] = {
      remaining match {
        case Nil => acc.reverse
        case head :: tail =>
          // Policz ile razy head się powtarza na początku
          @tailrec
          def countRepeats(lst: List[A], current: A, count: Int): (Int, List[A]) = {
            lst match {
              case Nil => (count, Nil)
              case h :: t if h == current => countRepeats(t, current, count + 1)
              case _ => (count, lst)
            }
          }
          
          val (count, rest) = countRepeats(tail, head, 1)
          compressHelper(rest, (head, count) :: acc)
      }
    }
    compressHelper(list, List.empty[(A, Int)])
  }
  
  // Zadanie 4: IsSub - sprawdź czy wszystkie elementy lSub są w l
  def isSub[A](l: List[A], lSub: List[A]): Boolean = {
    @tailrec
    def isSubHelper(remaining: List[A], toFind: List[A]): Boolean = {
      toFind match {
        case Nil => true
        case head :: tail =>
          if (contains(remaining, head)) isSubHelper(remaining, tail)
          else false
      }
    }
    
    @tailrec
    def contains(lst: List[A], elem: A): Boolean = {
      lst match {
        case Nil => false
        case h :: t if h == elem => true
        case _ :: t => contains(t, elem)
      }
    }
    
    isSubHelper(l, lSub)
  }
  
  // Zadanie 5: Compute - oblicz wartość listy Option używając dwóch operacji
  def compute[A, B](l: List[Option[A]])(op1: A => B)(op2: (A, B) => B): Option[B] = {
    @tailrec
    def computeHelper(remaining: List[Option[A]], acc: Option[B]): Option[B] = {
      remaining match {
        case Nil => acc
        case head :: tail =>
          head match {
            case None => computeHelper(tail, acc)
            case Some(value) =>
              acc match {
                case None => computeHelper(tail, Some(op1(value)))
                case Some(accumulated) => computeHelper(tail, Some(op2(value, accumulated)))
              }
          }
      }
    }
    computeHelper(l, None)
  }
  
  // Main - testy
  def main(args: Array[String]): Unit = {
    println("=== Zadanie 1: divide ===")
    val test1 = List(1, 3, 5, 6, 7)
    val result1 = divide(test1)
    println(s"divide($test1) = $result1")
    println(s"Oczekiwane: (List(1, 5, 7), List(3, 6))")
    println()
    
    println("=== Zadanie 2: merge ===")
    val a = List(1, 3, 5, 8)
    val b = List(2, 4, 6, 8, 10, 12)
    val result2 = merge(a, b)((m, n) => m < n)
    println(s"merge($a, $b) = $result2")
    println(s"Oczekiwane: List(1, 2, 3, 4, 5, 6, 8, 8, 10, 12)")
    println()
    
    println("=== Zadanie 3: compress ===")
    val test3 = List('a', 'a', 'b', 'c', 'c', 'c', 'd', 'd', 'c')
    val result3 = compress(test3)
    println(s"compress($test3) = $result3")
    println(s"Oczekiwane: List((a,2), (b,1), (c,3), (d,2), (c,1))")
    println()
    
    println("=== Zadanie 4: isSub ===")
    val l = List('b', 'o', 'c', 'i', 'a', 'n')
    val lSub = List('a', 'b', 'c')
    val result4 = isSub(l, lSub)
    println(s"isSub($l, $lSub) = $result4")
    println(s"Oczekiwane: true")
    println()
    
    println("=== Zadanie 5: compute ===")
    val test5 = List(Some(1), None, Some(2), None, Some(3), Some(4))
    val result5 = compute(test5)(_ + 0)(_ + _)
    println(s"compute($test5) = $result5")
    println(s"Oczekiwane: Some(10)")
    println()
    
    // Dodatkowe testy
    println("=== Dodatkowe testy ===")
    val allNone = List(None, None, None)
    val result5b = compute[Int, Int](allNone)(_ + 0)(_ + _)
    println(s"compute(wszystkie None) = $result5b")
    println(s"Oczekiwane: None")
  }
}
```

**Kluczowe koncepcje:**
- Pattern matching na listach (Nil, head :: tail)
- Funkcje generyczne [A], [A, B]
- Rekurencja ogonowa
- Currying
- Option type
- Tuple types

---

## LABORATORIUM 4: Metody kolekcji

### Pełny kod:

```scala
import scala.annotation.tailrec

object Main {

  // Zadanie 1 (lista - podział na indeksy parzyste i nieparzyste)
  def divide[A](list: List[A]): (List[A], List[A]) = {
    @tailrec
    def loop(xs: List[A], evenAcc: List[A], oddAcc: List[A], idxEven: Boolean): (List[A], List[A]) = xs match {
      case Nil => (evenAcc.reverse, oddAcc.reverse)
      case h :: t if idxEven => loop(t, h :: evenAcc, oddAcc, !idxEven)
      case h :: t => loop(t, evenAcc, h :: oddAcc, !idxEven)
    }
    loop(list, Nil, Nil, idxEven = true)
  }

  // Zadanie 2 (merge dwóch posortowanych list według leq)
  def merge[A](a: List[A], b: List[A])(leq: (A, A) => Boolean): List[A] = {
    @tailrec
    def loop(x: List[A], y: List[A], acc: List[A]): List[A] = (x, y) match {
      case (Nil, Nil) => acc.reverse
      case (Nil, _ ) => (acc.reverse) ++ y
      case (_ , Nil) => (acc.reverse) ++ x
      case (hx :: tx, hy :: ty) =>
        if (leq(hx, hy)) loop(tx, y, hx :: acc)
        else loop(x, ty, hy :: acc)
    }
    loop(a, b, Nil)
  }

  // Zadanie 3 (compress — run-length encoding)
  def compress[A](list: List[A]): List[(A, Int)] = {
    @tailrec
    def loop(xs: List[A], cur: Option[(A, Int)], acc: List[(A, Int)]): List[(A, Int)] = xs match {
      case Nil => cur match {
        case None => acc.reverse
        case Some(v) => (v :: acc).reverse
      }
      case h :: t => cur match {
        case None => loop(t, Some((h, 1)), acc)
        case Some((v, n)) if v == h => loop(t, Some((v, n + 1)), acc)
        case Some((v, n)) => loop(t, Some((h, 1)), (v, n) :: acc)
      }
    }
    loop(list, None, Nil)
  }

  // Zadanie 4 (czy lSub jest podzbiorem elementów l)
  def isSub[A](l: List[A], lSub: List[A]): Boolean = {
    @tailrec
    def contains(xs: List[A], target: A): Boolean = xs match {
      case Nil => false
      case h :: t => if (h == target) true else contains(t, target)
    }

    @tailrec
    def loop(remaining: List[A]): Boolean = remaining match {
      case Nil => true
      case h :: t => if (contains(l, h)) loop(t) else false
    }

    loop(lSub)
  }

  // Zadanie 5 (compute dla listy Option[A])
  def compute[A, B](l: List[Option[A]])(op1: A => B)(op2: (A, B) => B): Option[B] = {
    @tailrec
    def loop(xs: List[Option[A]], acc: Option[B]): Option[B] = xs match {
      case Nil => acc
      case None :: t => loop(t, acc)
      case Some(a) :: t => acc match {
        case None => loop(t, Some(op1(a)))
        case Some(b) => loop(t, Some(op2(a, b)))
      }
    }
    loop(l, None)
  }

  // --- Kolekcje (zadania z for/yield, fold, sliding, itp.) ---

  // Zadanie 1 (subSeq przy użyciu drop i take)
  def subSeq[A](seq: Seq[A], begIdx: Int, endIdx: Int): Seq[A] = {
    if (begIdx < 0 || endIdx < begIdx) Seq.empty
    else seq.drop(begIdx).take(endIdx - begIdx + 1)
  }

  // Zadanie 2 (remElem - usuwa k-ty element)
  def remElem[A](seq: Seq[A], k: Int): Seq[A] = {
    seq.zipWithIndex.filter { case (_, i) => i != k }.map(_._1)
  }

  // Zadanie 3 (diff - porównanie wg indeksów)
  def diff[A](seq1: Seq[A], seq2: Seq[A]): Seq[A] = {
    seq1.zipWithIndex.collect {
      case (v, i) if (if (i < seq2.length) seq2(i) != v else true) => v
    }
  }

  // Zadanie 4 (sumOption przy użyciu fold)
  def sumOption(seq: Seq[Option[Double]]): Double = {
    seq.foldLeft(0.0)((acc, o) => acc + o.getOrElse(0.0))
  }

  // Zadanie 5 (deStutter — usuwa powtarzające się ciągi)
  def deStutter[A](seq: Seq[A]): Seq[A] = {
    seq.foldLeft(Seq.empty[A]) { (acc, x) =>
      if (acc.isEmpty || acc.last != x) acc :+ x
      else acc
    }
  }

  // Zadanie 6 (isOrdered przy użyciu sliding)
  def isOrdered[A](seq: Seq[A])(leq: (A, A) => Boolean): Boolean = {
    seq.sliding(2).forall { pair => pair.size < 2 || leq(pair(0), pair(1)) }
  }

  // Zadanie 7 (freq przy użyciu groupBy)
  def freq[A](seq: Seq[A]): Set[(A, Int)] = {
    seq.groupBy(identity).map { case (k, v) => (k, v.length) }.toSet
  }

  // Zadanie 8 (median)
  def median(seq: Seq[(String, Double)]): Double = {
    val scores = seq.sortBy(_._2).map(_._2)
    val n = scores.length
    if (n == 0) Double.NaN
    else if (n % 2 == 1) scores(n / 2)
    else (scores(n / 2 - 1) + scores(n / 2)) / 2.0
  }

  // Zadanie 9 (minMax — imiona min i max punktów)
  def minMax(seq: Seq[(String, Double)]): Option[(String, String)] = {
    if (seq.isEmpty) None
    else {
      val min = seq.minBy(_._2)._1
      val max = seq.maxBy(_._2)._1
      Some((min, max))
    }
  }

  // Zadanie 10 (threeNumbers - trzy liczby spełniające twierdzenie Pitagorasa w zakresie [1,n])
  def threeNumbers(n: Int): Set[(Int, Int, Int)] = {
    (for {
      a <- 1 to n
      b <- (a + 1) to n
      c <- 1 to n
      if a * a + b * b == c * c
    } yield (a, b, c)).toSet
  }

  // Prosty main do demonstracji przykładów
  def main(args: Array[String]): Unit = {
    println("--- Zadania: pierwsza grupa ---")
    println("divide(List(1,3,5,6,7)) -> " + divide(List(1, 3, 5, 6, 7)))
    println("merge example -> " + merge(List(1,3,5,8), List(2,4,6,8,10,12))(_ < _))
    println("compress example -> " + compress(List('a','a','b','c','c','c','d','d','c')))
    println("isSub example -> " + isSub(List('b','o','c','i','a','n'), List('a','b','c')))
    val comp = compute(List(Some(1), None, Some(2), None, Some(3), Some(4)))(_ + 0)(_ + _)
    println("compute example -> " + comp)

    println("--- Zadania: kolekcje ---")
    println("subSeq -> " + subSeq(Seq(1,2,3,4,5), 1, 3))
    println("remElem -> " + remElem(Seq(1,2,3,4), 2))
    println("diff -> " + diff(Seq(1,2,3), Seq(2,2,1,3)))
    println("sumOption -> " + sumOption(Seq(Some(5.4), Some(-2.0), Some(1.0), None, Some(2.6))))
    println("deStutter -> " + deStutter(Seq(1,1,2,4,4,4,1,3)))
    println("isOrdered (<) -> " + isOrdered(Seq(1,2,2,4))(_ < _))
    println("isOrdered (<=) -> " + isOrdered(Seq(1,2,2,4))(_ <= _))
    println("freq -> " + freq(Seq('a','b','a','c','c','a')))
    val seqScores = Seq(("alice", 3.5), ("bob", 7.0), ("carol", 5.0))
    println("median -> " + median(seqScores))
    println("minMax -> " + minMax(seqScores))
    println("threeNumbers(20) -> " + threeNumbers(20).take(10))
  }
}
```

**Kluczowe koncepcje:**
- drop, take - pobieranie podciągów
- zipWithIndex - parowanie z indeksami
- zip - łączenie dwóch sekwencji
- foldLeft/foldRight - redukcja kolekcji
- sliding - okna przesuwne
- groupBy - grupowanie
- sortBy, minBy, maxBy - sortowanie i ekstrema
- for/yield - list comprehension
- collect - filtrowanie + mapowanie

---

## LABORATORIUM 5: Metody kolekcji - zadania praktyczne

### Pełny kod:

```scala
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
```

**Kluczowe koncepcje:**
- distinct - unikatowe elementy
- grouped - dzielenie na grupy
- collect - filtrowanie z pattern matching
- count - zliczanie
- patch - zastępowanie fragmentu sekwencji
- Case classes do modelowania danych
- Kompleksowe sortowanie (sortBy z tuplami)
- Algorytm rankingowy z miejscami ex-aequo

---

## LABORATORIUM 6: Przetwarzanie plików

### Zadanie 1: Histogram liter

**Opis:** Wczytanie pliku tekstowego i utworzenie histogramu częstości występowania liter.

**Kod:**
```scala
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
```

**Kluczowe koncepcje:**
- io.Source.fromResource - czytanie plików z resources
- getLines - iteracja po liniach
- filter(_.isLetter) - filtrowanie znaków
- groupBy(identity) - grupowanie identycznych elementów
- mapValues - transformacja wartości w mapie
- Skalowanie histogramu

---

### Zadanie 2: Analiza wyników wyborów

**Opis:** Wczytanie danych CSV z wynikami wyborów i analiza wyników po województwach.

**Kod:**
```scala
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
```

**Kluczowe koncepcje:**
- Parsowanie CSV
- split(",") - dzielenie stringów
- Case classes do reprezentacji danych
- Filtrowanie i agregacja danych
- Obliczenia procentowe
- Formatowanie wyniku (f-interpolacja)

---

## PODSUMOWANIE KLUCZOWYCH KONCEPTÓW SCALA

### 1. **Pattern Matching**
```scala
value match {
  case pattern1 => result1
  case pattern2 if condition => result2
  case _ => default
}
```

### 2. **Listy**
```scala
// Konstrukcja
val list = List(1, 2, 3)
val list2 = 1 :: 2 :: 3 :: Nil

// Pattern matching
list match {
  case Nil => "pusta"
  case head :: tail => s"głowa: $head"
}
```

### 3. **Rekurencja ogonowa**
```scala
@tailrec
def function(params, accumulator): Type = {
  if (baseCase) accumulator
  else function(newParams, newAccumulator)
}
```

### 4. **Funkcje wyższego rzędu**
```scala
def higher(f: A => B): C = ???
def curried(a: A)(b: B): C = ???
```

### 5. **Metody kolekcji**
- **map** - transformacja elementów
- **filter** - filtrowanie
- **flatMap** - mapowanie + spłaszczanie
- **fold** - redukcja (foldLeft/foldRight)
- **zip** - łączenie dwóch kolekcji
- **groupBy** - grupowanie
- **sliding** - okna przesuwne

### 6. **For comprehension**
```scala
for {
  x <- collection1
  y <- collection2
  if condition
} yield expression
```

### 7. **Option Type**
```scala
val opt: Option[Int] = Some(5)  // lub None
opt.getOrElse(default)
```

### 8. **Case Classes**
```scala
case class Person(name: String, age: Int)
// Automatycznie: equals, hashCode, toString, copy
```

---

## Instrukcje uruchamiania

### Kompilacja i uruchomienie (Scala 3):
```bash
# Pojedynczy plik
scala run plik.scala

# Projekt SBT
cd projekt
sbt run

# Kompilacja
sbt compile
```

---

**Dokument przygotowany: 9 grudnia 2025**
**Wszystkie rozwiązania przetestowane i działające**
