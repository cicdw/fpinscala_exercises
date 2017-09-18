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

  def addOne(as: MyList[Int]): MyList[Int] = {
    MyList.map(as)(_ + 1)
  }

  def toString(as: MyList[Double]): MyList[String] = {
    MyList.map(as)(_.toString)
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

  def sumLists(l: MyList[Int], r: MyList[Int]): MyList[Int] = {
    (l, r) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(a, lt), Cons(b, rt)) => Cons(a + b, sumLists(lt, rt))
    }
  }

  def zipWith[A, B, C](l: MyList[A], r: MyList[B])(f: (A, B) => C): MyList[C] = {
    (l, r) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(a, lt), Cons(b, rt)) => Cons(f(a, b), zipWith(lt, rt)(f))
    }
  }

  def hasSubsequence[A](sub: MyList[A], sup: MyList[A]): Boolean = {
    
    def shorten[A](elem: A, sl: MyList[A]): MyList[A] = {
      val dropped = dropWhile(sl, (x: A) => (x != elem)) // drops everything up to first occurence of elem
      
      dropped match {
        case Cons(h, t) => t // remove the matching element
        case _ => Nil
      }
    }

    (sub, sup) match {
      case (Cons(a, Nil), Cons(b, Nil)) => if (a == b) true else false
      case (Nil, _) => true
      case (_, Nil) => false
      case (Cons(h, t), _) => hasSubsequence(t, shorten(h, sup))
    }
    }
}

// TREES
//
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = {
//    t match {
//      case Leaf(_) => 1
//      case Branch(left, right) => size(left) + size(right)
//    }
    fold(t)((a: A) => 1)(_ + _)
  }

  def maximum(t: Tree[Int]): Int = {
//    t match {
//      case Leaf(num) => num
//      case Branch(left, right) => maximum(left) max maximum(right) // not in tail position
//    }
    fold(t)(identity)(_.max(_))
  }

  def depth[A](t: Tree[A]): Int = {
//    t match {
//      case Leaf(_) => 0
//      case Branch(left, right) => 1 + depth(left).max(depth(right))
//    }
    fold(t)((a: A) => 0)(1 + _.max(_))
  }

  def map[A, B](t: Tree[A], f: A => B): Tree[B] = {
//    t match {
//      case Leaf(a) => Leaf(f(a))
//      case Branch(left, right) => Branch(map(left, f), map(right, f))
//    }
    val g = (a: A) => Leaf(f(a)): Tree[B] // type annotation is required here
    fold(t)(g)(Branch(_, _))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(agg: (B, B) => B): B = {
    // takes tree, function on the values of the tree, and an aggregation method
    // for combining two leaves; "lifts" the function f via agg
    t match {
      case Leaf(a) => f(a)
      case Branch(left, right) => agg(fold(left)(f)(agg), fold(right)(f)(agg))
    }
  } 
}
