package com.fpinscala.exercise2

object Exercise2dot2 {

  def format_test_sorted(a: Array[Int]): Unit = {
    println("The array given by")
    a.foreach(arg => print("%d ".format(arg)))
    if (isSorted(a, ordered_ints)) println("is sorted.")
    else println("is NOT sorted.")
  }
  
  def isSorted[A](a: Array[A], ordered: (A, A) => Boolean): Boolean = {
   def loop(n: Int): Boolean = {
    if (n >= a.length) true
    else if (!ordered(a(n - 1), a(n))) false
    else loop(n + 1)
   }

   loop(1)
  }
  
  def ordered_ints(i: Int, j: Int): Boolean = {
    if (i <= j) true
    else false
  }

  def main(args: Array[String]): Unit = {
    val is_sorted = Array(5, 6, 7, 8, 9)
    val is_not_sorted = Array(2, 3, 4, 1, 8)
    val is_also_not_sorted = Array(2, 3, 4, 5, 4)

    format_test_sorted(is_sorted)
    format_test_sorted(is_not_sorted)
    format_test_sorted(is_also_not_sorted)
  }
}
