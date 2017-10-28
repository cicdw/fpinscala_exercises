package com.fpinscala.exercise4

// hide standard library Option / Either names
import scala.{Option => _, Either => _, _}


sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
