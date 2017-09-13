object Exercise2dot5 {

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
  
  def invert(x: Double): Double = 1 / x

  def main(args: Array[String]): Unit = {
    val x: Double = 2
    println("compose(invert, invert)(%f) = %f".format(x, compose(invert, invert)(x)))
  }

}
