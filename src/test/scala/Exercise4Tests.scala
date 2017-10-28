package com.fpinscala.exercise4

import org.scalatest.FunSuite

// hide standard library Option / Either
import scala.{Option => _, Either => _, _}

class Exercise4Tests extends FunSuite {

  test("test Option map works correctly") { 
    val x = 0.0f
    val y = 2.0f

    def f(a: Float): Option[Float] = {
      if (a == 0) None
      else Some(1 / a)
    }

    assert(f(x) == None)
    assert(f(y) == Some(0.5f))
  }

  test("4.1) Option.map") {
    val x = Option(5)
    val resx = x.map(_ + 1)
    val y = None
    val resy = y.map((a: Int) => a + 1)
    
    assert(resx == Option(6))
    assert(resy == None)
  }

  test("4.1) Option.getOrElse with default of different type") {
    val x = Option(5)
    val y = None
    // seems like this should break the compiler?
    val getx = x.getOrElse("No")
    val gety = y.getOrElse("No")

    assert(getx == 5)
    assert(gety == "No")
  }

//  test("4.1) Option.flatMap") {
//  
//  }
//
//
//  test("4.1) Option.orElse") {
//  
//  }
//
//  test("4.1) Option.filter") {
//  
//  }
}
