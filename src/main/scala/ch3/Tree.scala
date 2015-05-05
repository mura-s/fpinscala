package ch3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // ex3.25
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  // ex3.26
  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(n) => n
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  // ex3.27
  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  // ex3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  // ex3.29
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximumFold(t: Tree[Int]): Int =
    fold(t)(n => n)(_ max _)

  def depthFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((i,j) => 1 + (i max j))

  def mapFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)):Tree[B])(Branch(_,_))
}