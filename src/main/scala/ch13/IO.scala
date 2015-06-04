package ch13

import language._
import ch11._

object IO3 {

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] =
      FlatMap(this, f)

    def map[B](f: A => B): Free[F, B] =
      flatMap(f andThen (Return(_)))
  }

  case class Return[F[_], A](a: A) extends Free[F, A]

  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  // ex13.1
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] =
    new Monad[({type f[a] = Free[F, a]})#f] {
      def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa flatMap f

      def unit[A](a: => A): Free[F, A] = Return(a)
    }

  // ex13.2
  //  @annotation.tailrec
  //  def runTrampoline[A](a: Free[Function0, A]): A = a match {
  //    case Return(a1) => a1
  //    case Suspend(s1) => s1()
  //    case FlatMap(x1, f1) => x1 match {
  //      case Return(a2) => runTrampoline(f1(a2))
  //      case Suspend(s2) => runTrampoline(f1(s2()))
  //      case FlatMap(x2, f2) => runTrampoline(x2 flatMap (x3 => f2(x3) flatMap f1))
  //    }
  //  }

  // ex13.3
//  @annotation.tailrec
//  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
//    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
//    case FlatMap(Return(x), f) => step(f(x))
//    case _ => a
//  }
//
//  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
//    case Return(a) => F.unit(a)
//    case Suspend(r) => r
//    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
//    case _ => sys.error("Impossible")
//  }

  // ex13.4, 13.5: pass
}
