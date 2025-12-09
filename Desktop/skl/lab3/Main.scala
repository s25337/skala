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
