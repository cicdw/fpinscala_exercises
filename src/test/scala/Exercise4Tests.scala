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
    val x = MyOption(5)
    val resx = x.map(_ + 1)
    val y = MyNone
    val resy = y.map((a: Int) => a + 1)
    
    assert(resx == MyOption(6))
    assert(resy == MyNone)
  }

  test("4.1) MyOption.getOrElse with default of different type") {
    val x = MyOption(5)
    val y = MyNone
    
    // common supertype is Any
    val getx = x.getOrElse("No")
    val gety = y.getOrElse("No")

    assert(getx == 5)
    assert(gety == "No")
  }

  test("4.1) MyOption.getOrElse with default which is strict subtype") {
    val x = MyOption(5.0f)
    val a: Int = 5
    val getx = x.getOrElse(a)
    assert(getx == 5.0f)
  }
  
  test("4.1) MyOption.flatMap") {
    val x = MyOption(0.5)
    val y = MyOption(0.0)
    val n = MyNone

    def g(a: Double): MyOption[Double] = {
      if (a == 0) MyNone
      else MySome(1 / a)
    }

    assert (x.flatMap(g) == MyOption(2.0))
    assert (y.flatMap(g) == MyNone)
    assert (n.flatMap(g) == MyNone)
  }

//
//  test("4.1) MyOption.orElse") {
//  
//  }
//
//  test("4.1) MyOption.filter") {
//  
//  }
}
