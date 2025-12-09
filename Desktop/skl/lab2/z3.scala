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
