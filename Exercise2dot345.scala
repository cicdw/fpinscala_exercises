/* Haskel Curry */

object Exercise2dot345 {

  def sum(x: Int, y: Int): Int = x + y

  def partial[A, B, C](f: (A, B) => C, a: A): B => C = {
    b => f(a, b)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => (partial(f, a))
  }
  
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }


  def main(args: Array[String]): Unit = {
    println("f = sum(x, y)")
    val curry_sum = curry(sum)
    val curry_msg = "curry(sum)(%d)(%d) = %d"
    val uncurry_msg = "uncurry(curry(sum))(%d, %d) = %d"
    println(curry_msg.format(0, 2, curry_sum(0)(2)))
    println(curry_msg.format(54, 3, curry_sum(54)(3)))
    println(uncurry_msg.format(0, 2, curry_sum(0)(2)))
    println(uncurry_msg.format(54, 3, curry_sum(54)(3)))
  }

}
