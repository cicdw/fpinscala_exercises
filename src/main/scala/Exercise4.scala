package com.fpinscala.exercise4

// hide standard library Option / Either names
import scala.{Option => _, Either => _, Some => _, _}


sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(a) => MySome(f(a))
  }
  
  // if you don't specify output type, scala defaults to Any
  def getOrElse[B >: A](default: B): B = this match {
    case MyNone => default
    case MySome(a) => a
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(a) => f(a)
  }

  def filter(f: A => Boolean): MyOption[A] = {
    val isIt = this.map(f).getOrElse(false)
    if (isIt) this
    else MyNone
  }

//  def orElse[B >: A](ob: MyOption[B]): MyOption[B] = {
//    this.map(MySome(_)).getOrElse(ob)
//  }

  def orElse[B >: A](ob: MyOption[B]): MyOption[B] =  this match {
    case MyNone => ob
    case _ => this
  }
}
case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]


object MyOption {

  def mean(xs: Seq[Double]): MyOption[Double] = {
    if (xs.isEmpty) MyNone
    else MySome(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): MyOption[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = {
    a flatMap (aa => b map ((i: B) => f(aa, i)))
  }

  def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] = {
    a.foldRight[MyOption[List[A]]](MySome(Nil))((i, j) => map2(i, j)(_ :: _))
  }

  def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] = a match {
    case Nil => MySome(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }
}
