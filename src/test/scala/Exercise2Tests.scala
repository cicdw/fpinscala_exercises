package com.fpinscala.exercise2

import org.scalatest.FunSuite

class Exercise2Tests extends FunSuite {

    // Exercise 2.1
    test("2.1) fib(0) == 0") { 
        assert(Exercise2dot1.fib(0) == 0)
    }

    test("2.1) fib(1) == 1") { 
        assert(Exercise2dot1.fib(1) == 1)
    }

    test("2.1) The first 6 Fibonacci numbers are correct.") { 
        val inputs = Seq.range(0, 6)
        val outputs = inputs.map(i => Exercise2dot1.fib(i))
        assert(outputs == Seq(0, 1, 1, 2, 3, 5))
    }

    // Exercise 2.2
    test("2.2) Test isSorted returns True for sorted Ints.") { 
        val is_sorted = Array(5, 6, 7, 8, 9)
        val ans = Exercise2dot2.isSorted(is_sorted, Exercise2dot2.ordered_ints)
        assert(ans == true)
    }

    test("2.2) Test isSorted returns False for non-sorted Ints.") { 
        val is_not_sorted = Array(2, 3, 4, 1, 8)
        val is_also_not_sorted = Array(2, 3, 4, 5, 4)
        val ans1 = Exercise2dot2.isSorted(is_not_sorted, Exercise2dot2.ordered_ints)
        val ans2 = Exercise2dot2.isSorted(is_also_not_sorted, Exercise2dot2.ordered_ints)
        assert(!(ans1 | ans2) == true)
    }

    // Exercise 2.3
    test("2.3) Test curry(sum)(0)(2) == 2") {
        val curry_sum = Exercise2dot345.curry(Exercise2dot345.sum)
        assert(curry_sum(0)(2) == 2)
    }

    test("2.3) Test curry(sum)(54)(3) == 57") {
        val curry_sum = Exercise2dot345.curry(Exercise2dot345.sum)
        assert(curry_sum(54)(3) == 57)
    }

    test("2.4) Test uncurry(curry(sum))(54, 3) == 57") {
        val curry_sum = Exercise2dot345.curry(Exercise2dot345.sum)
        val uncurry_sum = Exercise2dot345.uncurry(curry_sum)
        assert(uncurry_sum(54, 3) == 57)
    }

    test("2.5) Test compose(invert, invert)(2.0) == 2.0") {
        val composed = Exercise2dot345.compose(Exercise2dot345.invert, Exercise2dot345.invert)
        assert(composed(2.0) == 2.0)
    }
}
