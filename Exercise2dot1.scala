/* should add tests so you can refine these exercises
 * and use CI on GitHub to ensure you can refactor */

object Exercise2dot1 {

  def fib(n: Int): Int = {
  @annotation.tailrec
  def accumulate(count: Int, prev: Int, curr: Int): Int = 
    if (count == 2) prev + curr
    else accumulate(count - 1, curr, prev + curr)

  if (n <= 1) n
  else accumulate(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    val num_to_compute = 6
    val output = Seq.range(0, num_to_compute)
    println("The first %d Fibonacci numbers are:".format(num_to_compute))
    output.foreach(i => print("%d ".format(fib(i))))
    println()
  }
}
