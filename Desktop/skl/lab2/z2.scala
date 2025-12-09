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