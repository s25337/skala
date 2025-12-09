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
