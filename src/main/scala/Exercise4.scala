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

  def orElse[B >: A](ob: MyOption[B]): MyOption[B] =  this match {
    case MyNone => ob
    case _ => this
  }
}
case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]
