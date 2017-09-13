//* will write this exercise as a script *//

sealed trait List[+A] // creating our own List data type

// define a few case classes which extend List
case object Nil extends List[Nothing] // whenever list is instantiated with nothing
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // "variadic" function 
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    // no error handling
    if (n == 1) tail(l)
    else drop(tail(l), n - 1)
  }
 
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
      case _ => l
    } 
  }
  
  def setHead[A](list: List[A], head: A): List[A] = Cons(head, list)

  def init[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A, B](as: List[A], b: B)(f: (A, B) => B): B =
    as match {
      case Nil => b
      case Cons(x, xs) => f(x ,foldRight(xs, b)(f))
    }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((list_val, count) => count + 1)
  }

  def foldLeft[A, B](as: List[A], b: B)(f: (B, A) => B): B = {
    as match {
      case Nil => b
      case Cons(h, t) => foldLeft(t, f(b, h))(f)
    }
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil:List[A])(setHead)
  }

  def append[A](as: List[A], z: List[A]): List[A] = {
    // appends one list to another
    foldRight(as, z)(Cons(_, _))
  }

  def concat[A](as: List[List[A]]): List[A] = {
    foldRight(as, Nil:List[A])(append)
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    as match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
    }
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldLeft(as, Nil:List[B])((acc, pop) => append(acc, f(pop)))
  }

  def filterviaflatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = {
    (l, r) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(a, lt), Cons(b, rt)) => Cons(f(a, b), zipWith(lt, rt)(f))
    }
  }
}


def sum_lists(l: List[Int], r: List[Int]): List[Int] = {
  // assume they are the same length
  (l, r) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(a, lt), Cons(b, rt)) => Cons(a + b, sum_lists(lt, rt))
  }
}


def add_one(as: List[Int]): List[Int] = {
  List.map(as)(_ + 1)
}


def to_string(as: List[Double]): List[String] = {
  List.map(as)(_.toString)
}

// eyeball "tests"
val x = List(1, 2, 3, 4, 5)
val y = List(1.0, 2.0, 3.0)
val less_than_3 = (x: Int) => (x < 3)
println("=" * 50)
println(s"The sum of $x is ${List.sum(x)}")
println(s"The product of $y is ${List.product(y)}")
println(s"The tail of $x is ${List.tail(x)}")
println(s"Appending 2 as head of $x is ${List.setHead(x, 2)}")
println(s"Appending 2.0 as head of $y is ${List.setHead(y, 2.0)}")
println(s"Dropping 2 from head of $x is ${List.drop(x, 2)}")
println(s"Dropping while < 3 from head of $x is ${List.dropWhile(x, less_than_3)}")
println(s"All but the last element of $x is ${List.init(x)}")
println(s"The length of $x is ${List.length(x)}")
println(s"The length of $y is ${List.length(y)}")
println(s"The (foldLeft) product of $x is ${List.foldLeft(x, 1)(_ * _)}")
println(s"The reverse of $y is ${List.reverse(y)}")
println(s"Appending List(88, 99) to $x yields ${List.append(x, List(88, 99))}")
println(s"Adding one to each element of $x yields ${add_one(x)}")
println(s"Converting $y to strings yields ${to_string(y)}")
println(s"Removing odd #s from $x yields ${List.filter(x)(i => (i % 2) == 0)}")
println(s"List.concat(List(List(1), List(2))) = ${List.concat(List(List(1), List(2)))}")
println(s"Duplicate by 2 flatmap on ${List(1, 2)} = ${List.flatMap(List(1, 2))(i => List(i, i))}")
println(s"Adding List(1, 2) to List(5, 6) yields ${sum_lists(List(1, 2), List(5, 6))}")
println(s"")
println("=" * 50)
println()
