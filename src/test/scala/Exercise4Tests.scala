package com.fpinscala.exercise4

import org.scalatest.FunSuite

// hide standard library Option / Either
import scala.{Option => _, Either => _, Some => _, _}

class Exercise4Tests extends FunSuite {

  test("test MyOption map works correctly") { 
    val x = 0.0f
    val y = 2.0f

    def f(a: Float): MyOption[Float] = {
      if (a == 0) MyNone
      else MySome(1 / a)
    }

    assert(f(x) == MyNone)
    assert(f(y) == MySome(0.5f))
  }

  test("4.1) MyOption.map") {
    val x = MySome(5)
    val resx = x.map(_ + 1)
    val y = MyNone
    val resy = y.map((a: Int) => a + 1)
    
    assert(resx == MySome(6))
    assert(resy == MyNone)
  }

  test("4.1) MyOption.getOrElse with default of different type") {
    val x = MySome(5)
    val y = MyNone
    
    // common supertype is Any
    val getx = x.getOrElse("No")
    val gety = y.getOrElse("No")

    assert(getx == 5)
    assert(gety == "No")
  }

  test("4.1) MyOption.getOrElse with default which is strict subtype") {
    val x = MySome(5.0f)
    val a: Int = 5
    val getx = x.getOrElse(a)
    assert(getx == 5.0f)
  }
  
  test("4.1) MyOption.flatMap") {
    val x = MySome(0.5)
    val y = MySome(0.0)
    val n = MyNone

    def g(a: Double): MyOption[Double] = {
      if (a == 0) MyNone
      else MySome(1 / a)
    }

    assert (x.flatMap(g) == MySome(2.0))
    assert (y.flatMap(g) == MyNone)
    assert (n.flatMap(g) == MyNone)
  }

  test("4.1) MyOption.filter") {
    val x = MySome(3)
    val y = MySome(4)
    val z = MyNone
    val even = (i: Int) => (i % 2) == 0
    val filtx = x.filter(even)
    val filty = y.filter(even)
    val filtz = z.filter(even) 

    assert(filtx == MyNone)
    assert(filty == MySome(4))
    assert(filtz == MyNone)
  }

  test("4.1) MyOption.orElse") {
    val x = MySome(0.5)
    val y = MyNone
    val default = MySome(0.0)

    assert(x.orElse(default) == x)
    assert(y.orElse(default) == default)
  }

  test("4.2) Test Variance function on constant sequences") {
    val x = Seq(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
    val y = Seq(4.23, 4.23, 4.23, 4.23, 4.23, 4.23, 4.23)
    assert(MyOption.variance(x) == MySome(0))
    assert(MyOption.variance(y) == MySome(0))
  }

  test("4.2) Test Variance function on non-constant mean 0 sequences") {
    val x = Seq(1.0, -1.0)
    val y = Seq(-2.0, 0.0, 0.5, 1.5)
    assert(MyOption.variance(x) == MySome(1.0))
    assert(MyOption.variance(y) == MySome(1.625))
  }

  test("4.2) Test Variance function on non-constant sequences") {
    val x = Seq(1.0, -2.0)
    assert(MyOption.variance(x) == MySome(2.25))
  }

  test("4.2) Test Variance function on empty sequences") {
    val x = Seq()
    assert(MyOption.variance(x) == MyNone)
  }

  test("4.3) Test MyOption.map2") {
    val x = MySome(2) 
    val y = MySome(3)
    val f = (a: Int, b: Int) => a + b
    val out = MyOption.map2(x, y)(_ + _)
    assert(out == MySome(5))

    val a = MyNone
    val out2 = MyOption.map2(x, a)(f)
    assert(out2 == MyNone)

    val out3 = MyOption.map2(a, y)(f)
    assert(out3 == MyNone)

    val out4 = MyOption.map2(a, a)(f)
    assert(out4 == MyNone)
  }
}
