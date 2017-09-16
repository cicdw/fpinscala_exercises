package com.fpinscala.exercise3

//* will write this exercise as a script *//

sealed trait MyList[+A] // creating our own List data type

// define a few case classes which extend List
case object Nil extends MyList[Nothing] // whenever list is instantiated with nothing
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): MyList[A] = // "variadic" function 
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def tail[A](list: MyList[A]): MyList[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def drop[A](l: MyList[A], n: Int): MyList[A] = {
    // no error handling
    if (n == 1) tail(l)
    else drop(tail(l), n - 1)
  }
 
  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = {
    l match {
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
      case _ => l
    } 
  }
  
  def setHead[A](list: MyList[A], head: A): MyList[A] = Cons(head, list)

  def init[A](list: MyList[A]): MyList[A] = {
    list match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A, B](as: MyList[A], b: B)(f: (A, B) => B): B =
    as match {
      case Nil => b
      case Cons(x, xs) => f(x ,foldRight(xs, b)(f))
    }

  def length[A](as: MyList[A]): Int = {
    foldRight(as, 0)((list_val, count) => count + 1)
  }

  def foldLeft[A, B](as: MyList[A], b: B)(f: (B, A) => B): B = {
    as match {
      case Nil => b
      case Cons(h, t) => foldLeft(t, f(b, h))(f)
    }
  }

  def foldRightviafoldLeft[A, B](as: MyList[A], b: B)(f: (A, B) => B): B = {
    // the trick is to notice that the *first* computation of foldLeft will 
    // involve the *first* element of MyList[A] and b, whereas foldRight needs
    // the first computation to be the *last* element of MyList[A] and b.
    // Thus we simply build up the computation through function composition
    // until we are ready to evaluate:
    
    val id = (b: B) => b
    val composition = (current_func: B => B, next_val: A) => ((z: B) => current_func(f(next_val, z)))
    val built_up_computation = foldLeft(as, id)(composition)
    built_up_computation(b) // actually evaluate
  }
  
  def reverse[A](as: MyList[A]): MyList[A] = {
    foldLeft(as, Nil:MyList[A])(setHead)
  }

  def append[A](as: MyList[A], z: MyList[A]): MyList[A] = {
    // appends one list to another
    foldRight(as, z)(Cons(_, _))
  }

  def concat[A](as: MyList[MyList[A]]): MyList[A] = {
    foldRight(as, Nil:MyList[A])(append)
  }

  def map[A, B](as: MyList[A])(f: A => B): MyList[B] = {
    as match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }
  }

  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = {
    as match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
    }
  }

  def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] = {
    foldLeft(as, Nil:MyList[B])((acc, pop) => append(acc, f(pop)))
  }

  def filterviaflatMap[A](as: MyList[A])(f: A => Boolean): MyList[A] = {
    flatMap(as)((a:A) => if (f(a)) MyList(a) else Nil)
  }

  def zipWith[A, B, C](l: MyList[A], r: MyList[B])(f: (A, B) => C): MyList[C] = {
    (l, r) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(a, lt), Cons(b, rt)) => Cons(f(a, b), zipWith(lt, rt)(f))
    }
  }
}

//
//def sum_lists(l: MyList[Int], r: MyList[Int]): MyList[Int] = {
//  // assume they are the same length
//  (l, r) match {
//    case (_, Nil) => Nil
//    case (Nil, _) => Nil
//    case (Cons(a, lt), Cons(b, rt)) => Cons(a + b, sum_lists(lt, rt))
//  }
//}
//
//
//def add_one(as: MyList[Int]): MyList[Int] = {
//  MyList.map(as)(_ + 1)
//}
//
//
//def to_string(as: MyList[Double]): MyList[String] = {
//  MyList.map(as)(_.toString)
//}
//
//
//def assymetric_fun(a: Int, b: Int): Int = {
//  println(s"calling with $a lhs and $b rhs")
//  a + b
//}
//
//
//// eyeball "tests"
//val x = MyList(1, 2, 3, 4, 5)
//val y = MyList(1.0, 2.0, 3.0)
//val less_than_3 = (x: Int) => (x < 3)
//println("=" * 50)
//println(s"The sum of $x is ${MyList.sum(x)}")
//println(s"The product of $y is ${MyList.product(y)}")
//println(s"The tail of $x is ${MyList.tail(x)}")
//println(s"Appending 2 as head of $x is ${MyList.setHead(x, 2)}")
//println(s"Appending 2.0 as head of $y is ${MyList.setHead(y, 2.0)}")
//println(s"Dropping 2 from head of $x is ${MyList.drop(x, 2)}")
//println(s"Dropping while < 3 from head of $x is ${MyList.dropWhile(x, less_than_3)}")
//println(s"All but the last element of $x is ${MyList.init(x)}")
//println(s"The length of $x is ${MyList.length(x)}")
//println(s"The length of $y is ${MyList.length(y)}")
//println(s"The (foldLeft) product of $x is ${MyList.foldLeft(x, 1)(_ * _)}")
//println(s"The reverse of $y is ${MyList.reverse(y)}")
//println(s"Appending MyList(88, 99) to $x yields ${MyList.append(x, MyList(88, 99))}")
//println(s"Adding one to each element of $x yields ${add_one(x)}")
//println(s"Converting $y to strings yields ${to_string(y)}")
//println(s"Removing odd #s from $x yields ${MyList.filter(x)(i => (i % 2) == 0)}")
//println(s"MyList.concat(MyList(MyList(1), MyList(2))) = ${MyList.concat(MyList(MyList(1), MyList(2)))}")
//println(s"Duplicate by 2 flatmap on ${MyList(1, 2)} = ${MyList.flatMap(MyList(1, 2))(i => MyList(i, i))}")
//println(s"Adding MyList(1, 2) to MyList(5, 6) yields ${sum_lists(MyList(1, 2), MyList(5, 6))}")
//println(s"foldRightviafoldLeft(MyList(1, 2, 3, 4, 5))(assymetric_fun) = ${MyList.foldRightviafoldLeft(x, 0)(assymetric_fun)}")
//println(s"")
//println("=" * 50)
//println()
