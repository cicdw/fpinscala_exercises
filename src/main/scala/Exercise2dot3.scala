package com.fpinscala.exercise2

/* Haskel Curry */

object Ex2dot3 {

  def sum(x: Int, y: Int): Int = x + y

  def partial[A, B, C](f: (A, B) => C, a: A): B => C = {
    b => f(a, b)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => (partial(f, a))
  }
  
  def main(args: Array[String]): Unit = {
    println("f = sum(x, y)")
    val curry_sum = curry(sum)
    println("curry(sum)(%d)(%d) = %d".format(0, 2, curry_sum(0)(2)))
    println("curry(sum)(%d)(%d) = %d".format(54, 3, curry_sum(54)(3)))
  }

}
