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