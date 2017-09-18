package com.fpinscala.exercise3

import org.scalatest.FunSuite

class Exercise3Tests extends FunSuite {

  def fixture = 
    new {
      val x = MyList(1, 2, 3, 4, 5)
      val y = MyList(1.0, 2.0, 3.0)
      val t = Branch(Branch(Branch(Leaf(0), Leaf(0)), Branch(Leaf(2), Leaf(0))), Branch(Leaf(1), Leaf(1)))
    }

  test("MyList.sum with Ints") { 
    val x = fixture.x
    assert(MyList.sum(x) == 15)
  }

  test("MyList.product with Doubles") { 
    val y = fixture.y
    assert(MyList.product(y) == 6.0)
  }

  test("3.2) test tail") {
    val x = fixture.x
    assert(MyList.tail(x) == MyList(2, 3, 4, 5))
  }

  test("3.2) test tail(Nil) == Nil") {
    assert(MyList.tail(Nil) == Nil)
  }

  test("3.3) test setHead prepends with Ints") {
    val x = fixture.x
    assert(MyList.setHead(x, 2) == MyList(2, 1, 2, 3, 4, 5))
  }

  test("3.3) test setHead prepends with Doubles") {
    val y = fixture.y
    assert(MyList.setHead(y, 2.0) == MyList(2.0, 1.0, 2.0, 3.0))
  }

  test("3.4) test drop first 2 elements") {
    val x = fixture.x
    assert(MyList.drop(x, 2) == MyList(3, 4, 5))
  }

  test("3.5) test dropWhile with < 3.0") {
    val y = fixture.y
    val lessThan3 = (y: Double) => (y < 3.0)
    assert(MyList.dropWhile(y, lessThan3) == MyList(3.0))
  }

  test("3.5) test dropWhile with == 0") {
    val x = fixture.x
    val not0 = (x: Int) => (x == 0)
    assert(MyList.dropWhile(x, not0) == x)
  }

  test("3.6) test init removes the last element") {
    val x = fixture.x
    val y = fixture.y
    assert(MyList.init(x) == MyList(1, 2, 3, 4))
    assert(MyList.init(y) == MyList(1.0, 2.0))
  }

  test("3.9) test length w/ foldRight") {
    val x = fixture.x
    val y = fixture.y
    assert(MyList.length(x) == 5)
    assert(MyList.length(y) == 3)
  }

  test("3.10) test foldLeft w/ product") {
    val x = fixture.x
    val y = fixture.y
    assert(MyList.foldLeft(x, 1)(_ * _) == 120)
    assert(MyList.foldLeft(y, 1.0)(_ * _) == 6.0) // 1.0 needed for type inference on _ * _
  }

  test("3.11) test reverse reverses") {
    val x = fixture.x
    val y = fixture.y
    assert(MyList.reverse(y) == MyList(3.0, 2.0, 1.0)) 
    assert(MyList.reverse(x) == MyList(5, 4, 3, 2, 1)) 
  }

  test("3.13) foldRight by foldLeft'ing") {
    val x = fixture.x
    assert(MyList.foldRight(x, 0)(_ - _) == 3)
  }

  test("3.14) test append can append Lists") {
    val x = fixture.x
    val out = MyList.append(x, MyList(88, 99))
    assert(out == MyList(1, 2, 3, 4, 5, 88, 99))
  }

  test("3.15) test concat can 'flatten' lists") {
    val out = MyList.concat(MyList(MyList(1), MyList(2)))
    assert(out == MyList(1, 2))
  }

  test("3.16) test addOne") {
    val x = fixture.x
    val out = MyList.addOne(x)
    assert(out == MyList(2, 3, 4, 5, 6))
  }

  test("3.17) test toString") {
    val y = fixture.y
    val out = MyList.toString(y)
    assert(out == MyList("1.0", "2.0", "3.0"))
  }

  test("3.19) test filter by removing odd numbers") {
    val x = fixture.x
    val out = MyList.filter(x)(i => (i % 2) == 0)
    assert(out == MyList(2, 4))
  }

  test("3.20) test flatMap with duplication") {
    val x = fixture.x
    val out = MyList.flatMap(MyList(1, 2))(i => MyList(i, i))
    assert(out == MyList(1, 1, 2, 2))
  }

  test("3.21) test filter implemented with flatMap with odd number filter") {
    val x = fixture.x
    val out = MyList.filterviaflatMap(x)(i => (i % 2) == 0)
    assert(out == MyList(2, 4))
  }

  test("3.22) test sumLists with equal length integer lists") {
    val out = MyList.sumLists(MyList(1, 2), MyList(5, 6))
    assert(out == MyList(6, 8))
  }

  test("3.22) test sumLists with non-equal length integer lists returns shorter list") {
    val out = MyList.sumLists(MyList(1, 2), MyList(55))
    assert(out == MyList(56))
  }

  test("3.23) test zipWith sum passes same sumLists tests") {
    val diffLengthOut = MyList.zipWith(MyList(1, 2), MyList(55))(_ + _)
    val sameLengthOut = MyList.zipWith(MyList(1, 2), MyList(5, 6))(_ + _)
    assert(sameLengthOut == MyList(6, 8))
    assert(diffLengthOut == MyList(56))
  }

  test("3.24) test hasSubsequence on single element lists") {
   val x = MyList(5)
   val t = MyList(5)
   val f = MyList(3)
   assert(MyList.hasSubsequence(t, x))
  assert(!MyList.hasSubsequence(f, x))
  }

  test("3.24) test hasSubsequence on equal lists") {
    val x = fixture.x
    assert(MyList.hasSubsequence(x, x))
  }

  test("3.24) test hasSubsequence returns false on (1, 1, 2) and (1, 2, 3, 4, 5)") {
    val x = MyList(1, 1, 2)
    val y = MyList(1, 2, 3, 4, 5)
    assert(!(MyList.hasSubsequence(x, y)))
  }

  test("3.24) test hasSubsequence returns true on (1, 3, 2) and (1, 7, 3, 5, 3, 2)") {
    val x = MyList(1, 3, 2)
    val y = MyList(1, 7, 3, 5, 3, 2)
    assert(MyList.hasSubsequence(x, y))
  }

  test("3.24) test hasSubsequence returns false when sub sequence is longer than super") {
    val x = MyList(1, 1, 2) 
    val y = MyList(1, 1)
    assert(!MyList.hasSubsequence(x, y))
  }

  test("3.24) test hasSubsequence returns true when subsequence ends with last element") {
    val x = MyList(1, 3, 0)
    val y = MyList(1, 7, 3, 5, 3, 2, 0)
    assert(MyList.hasSubsequence(x, y))
  }

  test("3.25) test Tree.size() == 1 for leaves") {
    val l = Leaf(0)
    val q = Leaf("string")
    assert(Tree.size(l) == 1)
    assert(Tree.size(q) == 1)
  }

  test("3.25) test Tree.size() == 2 for Branch w/ 2 leaves") {
    val l = Branch(Leaf(0), Leaf("a different type"))
    assert(Tree.size(l) == 2)
  }

  test("3.25) test Tree.size() on a general tree") {
    val t = fixture.t
    assert(Tree.size(t) == 6)
  }
}
