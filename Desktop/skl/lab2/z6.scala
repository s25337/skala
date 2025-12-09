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

