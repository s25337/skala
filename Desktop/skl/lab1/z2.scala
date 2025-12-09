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
