package ch01;

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/*
companion object to hold functions for creating or working with
functional data types (datastructures)
**/
object List { 
  def apply[A](as: A*): List[A] = // accept a variadic as parameter
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*)) // pass the rest as a variadic to a function
    
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def product(doubles: List[Double]): Double = doubles match {
      case Nil => 1.0
      case Cons(head, tail) => head * product(tail)
  }

  def tail[A](ls: List[A]): List[A] = ls match {
      case Nil => Nil
      case Cons(head, tail) => tail
  }

  def setHead[A](ls: List[A], a: A): List[A] = ls match {
      case Nil => Cons(a, Nil)
      case Cons(head, tail) => Cons(a, tail)
  }

  def drop[A](ls: List[A], n: Int): List[A] = ls match {
      case Nil => Nil
      case Cons(head, tail) if n == 0 => Cons(head, tail)
      case Cons(head, tail) => drop(tail, n-1)
  }

  /**
    * 
    *
    * @param ls
    * @param f
    * @return
    * 
    * In this form of the function dropWhile, type information for the list is lost
    * hence the provided predicate needs to provide the type annotation on the param for f
    * 
    * for example 
    *       val list = List(1,2,3,4,5)
    *       dropWhile(l, (x: Int) => x < 2)
    * 
    * To avoid this defining dropWhile with grouped parmeters preserves the type information
    * this is,
    *       dropWhile[A](ls: List[A])(f: A => Boolean): List[A]
    * 
    * here we have 2 parameter groups and this allows the type information to flow through
    * and then usage with anonymous functions becomes
    *       dropWhile(l)(x => x < 2)
    */
  def dropWhile[A](ls: List[A], f: A => Boolean): List[A] = ls match {
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _ => ls
  }

  def init[A](ls: List[A]): List[A] = ls match {
      case Nil => Nil
      case Cons(head, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
  }
  
  def foldRight[A, B](ls: List[A], init: B)(f: (A, B) => B): B = ls match {
      case Nil => init
      case Cons(head, tail) => f(head, foldRight(tail, init)(f))
  }

  def foldLeft[A, B](ls: List[A], init: B)(f: (B, A) => B): B = ls match {
    case Nil => init
    case Cons(head, tail) => foldLeft(tail, f(init, head))(f)
  }

  def reverse[A](ls: List[A]): List[A] = foldLeft(ls, List[A]())((x, y) => Cons(y, x))

  def append[A](ls: List[A], lt: List[A]): List[A] = 
    foldRight(ls, lt)((x, y) => Cons(x, y))

  def map[A, B](ls: List[A])(f: A => B): List[B] = foldRight(ls, List[B]())((x, y) => Cons(f(x), y))

  def filter[A](ls: List[A])(f: A => Boolean): List[A] = foldRight(ls, List[A]())((x, y) => if (f(x)) Cons(x, y) else y)

  def flatMap[A, B](ls: List[A])(f: A => List[B]): List[B] = foldRight(ls, List[B]())((x, y) => append(f(x), y))

  def filterUsingFlatMap[A](ls: List[A])(f: A => Boolean): List[A] = flatMap(ls)(x => if (f(x)) List(x) else List())

  def zip[A](ls: List[A], lt: List[A])(f: (A, A) => A): List[A] = (ls, lt) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(f(head1, head2), zip(tail1, tail2)(f))
  }

  def length[A](ls: List[A]): Int = foldRight(ls, 0)((_, acc) => 1 + acc)
 }
