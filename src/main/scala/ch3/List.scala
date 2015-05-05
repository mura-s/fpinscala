package ch3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // ex3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("空のListにはtailできない")
    case Cons(_, xs) => xs
  }

  // ex3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("空のListにはsetHeadできない")
    case Cons(_, xs) => Cons(h, xs)
  }

  // ex3.4
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }

  // ex3.5
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  // ex3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("空のListにはinitできない")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // ex3.7: 返せない。0.0を検出した時点スタックに乗っている処理が実行されてしまう。
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // ex3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, n) => 1 + n)

  // ex3.10
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  // ex3.11
  def sumFl(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def productFl(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def lengthFl[A](as: List[A]): Int =
    foldLeft(as, 0)((acc, _) => acc + 1)

  // ex3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((as, a) => Cons(a, as))

  // ex3.13 pass

  // ex3.14
  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))

  // ex3.15
  def concat[A](ll: List[List[A]]): List[A] =
    foldRight(ll, Nil: List[A])(append(_, _))

  // ex3.16
  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((i, is) => Cons(i + 1, is))

  // ex3.17
  def dToS(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString(), t))

  // ex3.18
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  // ex3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  // ex3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil:List[B])((h,t) => append(f(h), t))
    // or concat(map(l)(f))

  // ex3.21
  def filterFM[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // ex3.22
  def zip(l: List[Int], r: List[Int]): List[Int] =
    (l,r) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(hl,tl), Cons(hr,tr)) => Cons(hl+hr, zip(tl,tr))
    }

  // ex3.23
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] =
    (a,b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(ha,ta), Cons(hb,tb)) => Cons(f(ha,hb), zipWith(ta,tb)(f))
    }

  // ex3.24 pass

}

object Main {
  def main(args: Array[String]) {
    // ex3.1
    val x1 = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y  // match
      case Cons(h, t) => h - h
      case _ => 101
    }
    println(x1)

    // ex3.8
    val x2 = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    println(x2)

    println(List.filterFM(List(1,2,3,4,5))(i => i % 2 == 0))
  }
}
