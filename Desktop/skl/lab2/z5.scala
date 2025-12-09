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