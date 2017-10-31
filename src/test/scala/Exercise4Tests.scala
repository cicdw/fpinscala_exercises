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

  test("4.4) Test MyOption.sequence") {
    val ll = List(MySome(1), MySome(2), MySome(5))
    val qq = List(MySome(1), MyNone, MySome(5))

    assert(MyOption.sequence(ll) == MySome(List(1, 2, 5)))
    assert(MyOption.sequence(qq) == MyNone)
  }

  test("4.5) Test MyOption.traverse") {
    val ll = List(1.0, 2.0, 5.0)
    val xx = List(0.5, 0.0, 1.13)

    def f(i: Double): MyOption[Double] = {
      if (i == 0) MyNone
      else MySome(1 / i)
    }

    assert(MyOption.traverse(ll)(f) == MySome(List(1.0, 0.5, 0.2)))
    assert(MyOption.traverse(xx)(f) == MyNone)
  }

  test("4.6) Test MyEither methods") {
    val xx = MyRight(2)
    val rr = MyRight(0)
    val nn = MyLeft("Zero")
    val qq = MyRight("True")

    def f(x: Int): String = x toString

    def flatF(x: Int): MyEither[String, Int] = {
      if (x == 0) MyLeft("Zero")
      else MyRight(x + 1)
    }

    def adder(x: Int, y: Int): String = (x + y) toString

    assert(xx.map(f) == MyRight("2"))
    assert(xx.flatMap(flatF) == MyRight(3))
    assert(rr.flatMap(flatF) == MyLeft("Zero"))
    assert(nn.orElse(qq) == qq)
    assert(xx.orElse(rr) == xx)
    assert(xx.map2(rr)(adder) == MyRight("2"))
    assert(xx.map2(nn)(adder) == nn)
    assert(nn.map2(xx)(adder) == nn)
  }
}
