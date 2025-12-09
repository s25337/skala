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
